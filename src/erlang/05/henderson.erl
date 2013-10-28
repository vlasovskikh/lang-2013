-module(henderson).
-author("vlan").
-mode(compile).
-include_lib("eunit/include/eunit.hrl").
-export([main/1]).


% The language of vector algebra
-type vector() :: {vector, number(), number()}.

-spec new_vector(X :: number(), Y :: number()) -> vector().
-spec x_coord(vector()) -> number().
-spec y_coord(vector()) -> number().

-spec add_vector(vector(), vector()) -> vector().
-spec subtract_vector(vector(), vector()) -> vector().
-spec scale_vector(vector(), number()) -> vector().


% Frame for drawing vector images
-type frame() :: {frame, vector(), vector(), vector()}.

-spec new_frame(Origin :: vector(),
                Horiz :: vector(),
                Vert :: vector()) -> frame().
-spec frame_origin(frame()) -> vector().
-spec frame_horiz(frame()) -> vector().
-spec frame_vert(frame()) -> vector().
-spec map_vector(frame(), vector()) -> vector().


% SVG element with the following set of ad hoc polymorphic operations
-type svg_operations() :: {
  svg_operations,
  ToSvg :: fun(() -> string()),
  MapToFrame :: fun((frame()) -> any_svg_element())
}.
-type svg_element(Name, Data) :: {Name, svg_operations(), Data}.
-type any_svg_element() :: svg_element(atom(), any()).


% Erlang doesn't have language support for ad hoc polymorphism, so we
% have to emulate it using a technique similar to virtual function
% tables of C++.


% Types of polymorphic functions on SVG elements
-spec to_svg(any_svg_element()) -> string().
-spec map_to_frame(svg_element(Name, Data), frame()) -> svg_element(Name, Data).


% Line is a scalable vector graphic (SVG) element, that represent a line
-type svg_line() :: svg_element(svg_line, {vector(), vector()}).

-spec new_svg_line(Start :: vector(), End :: vector()) -> svg_line().
-spec new_svg_line(X1 :: number(), Y1 :: number(),
                  X2 :: number(), Y2 :: number()) -> svg_line().


% Ellipse is another example of a polymorphic SVG element
-type svg_ellipse() :: svg_element(svg_ellipse, {vector(), number(), number()}).
-spec new_svg_ellipse(Center :: vector(), Rx :: number(), Ry :: number()) -> svg_ellipse().


% Picture is a function that when given a frame, returns a list of
% some elements representing the picture in this frame. Picture
% is the only primitive in Henderson-Escher language. Note that the
% picture abstracts away the type of its elements
-type picture(A) :: fun((frame()) -> [A]).


% Currently we deal only with pictures, that consist of SVG elements
-spec new_svg_picture([any_svg_element()]) -> picture(any_svg_element()).
-spec picture_to_svg(picture(any_svg_element()), frame(),
                     Width :: string(),
                     Height :: string()) -> string().


% Henderson-Escher language of pictures has some means of
% combination
-spec beside(picture(A), picture(A)) -> picture(A).
-spec above(picture(A), picture(A)) -> picture(A).
-spec flip_vert(picture(A)) -> picture(A).
-spec flip_horiz(picture(A)) -> picture(A).


% Invoke polymorphic functions on a SVG element
to_svg({_, {svg_operations, ToSvg, _}, _}) -> ToSvg().
map_to_frame({_, {svg_operations, _, MapToFrame}, _}, Frame) -> MapToFrame(Frame).


% Let's switch to implementations of these type specifications.


% Constructor and selectors of vector()
new_vector(X, Y) -> {vector, X, Y}.
x_coord({vector, X, _}) -> X.
y_coord({vector, _, Y}) -> Y.


% Implementation of vector algebra

add_vector(V1, V2) ->
  new_vector(x_coord(V1) + x_coord(V2),
             y_coord(V1) + y_coord(V2)).


subtract_vector(V1, V2) ->
  new_vector(x_coord(V1) - x_coord(V2),
             y_coord(V1) - y_coord(V2)).


scale_vector(V, N) ->
  new_vector(x_coord(V) * N,
             y_coord(V) * N).


invert_vector(V) ->
  scale_vector(V, -1.0).


% Constructor and selectors of frame()
new_frame(Origin, Horiz, Vert) -> {frame, Origin, Horiz, Vert}.
frame_origin({frame, Origin, _, _}) -> Origin.
frame_horiz({frame, _, Horiz, _}) -> Horiz.
frame_vert({frame, _, _, Vert}) -> Vert.


% The identity frame
identity_frame() ->
  new_frame(new_vector(0, 0),
            new_vector(1, 0),
            new_vector(0, 1)).


% Map a vector from the identity frame to the given frame
map_vector(Frame, Vector) ->
  % Origin + Horiz * X + Vert * Y
  add_vector(frame_origin(Frame),
             add_vector(scale_vector(frame_horiz(Frame),
                                     x_coord(Vector)),
                        scale_vector(frame_vert(Frame),
                                     y_coord(Vector)))).


% Constructors and operations of svg_line()
new_svg_line(Start, End) ->
  Data = {Start, End},
  {
    svg_line,
    {
      svg_operations,
      fun () ->
        io_lib:format("<line x1='~w' y1='~w' x2='~w' y2='~w' stroke-width='2'/>",
                      [
                        x_coord(Start),
                        y_coord(Start),
                        x_coord(End),
                        y_coord(End)
                      ])
      end,
      fun (Frame) ->
        new_svg_line(map_vector(Frame, Start),
                     map_vector(Frame, End))
      end
    },
    Data
  }.
new_svg_line(X1, Y1, X2, Y2) ->
  new_svg_line(new_vector(X1, Y1),
               new_vector(X2, Y2)).


% Constructor and operations of ellipse()
new_svg_ellipse(Center, Rx, Ry) ->
  Data = {Center, Rx, Ry},
  {
    svg_ellipse,
    {
      svg_operations,
      fun () ->
        io_lib:format("<ellipse cx='~w' cy='~w' rx='~w' ry='~w' fill='none' stroke-width='2'/>",
                      [
                        x_coord(Center),
                        y_coord(Center),
                        Rx,
                        Ry
                      ])
      end,
      fun (Frame) ->
        % XXX: This simple radius transformation works only for
        % rectangular frames
        new_svg_ellipse(map_vector(Frame, Center),
                        Rx * abs(x_coord(frame_horiz(Frame))),
                        Ry * abs(y_coord(frame_vert(Frame))))
      end
    },
    Data
  }.


% Creating a picture from a list of SVG elements means creating a function
% that when given a frame, maps each SVG element to this frame
new_svg_picture(Elements) ->
  fun (Frame) ->
    lists:map(
      fun (Element) ->
        map_to_frame(Element, Frame)
      end,
      Elements)
  end.


% Picture is a SVG image
picture_to_svg(Picture, Frame, Width, Height) ->
  Header = io_lib:format(
    "<svg width='~s' height='~s' viewBox='0 0 ~w ~w'~n"
    "     xmlns='http://www.w3.org/2000/svg' version='1.1'>~n"
    "  <g stroke='black'>~n",
    [
      Width,
      Height,
      x_coord(frame_horiz(Frame)),
      y_coord(frame_vert(Frame))
    ]),
  Elements = Picture(Frame),
  Body = string:join(lists:map(fun to_svg/1, Elements),
                     "\n"),
  Footer =
    "\n"
    "  </g>\n"
    "</svg>",
  Header ++ Body ++ Footer.


% This is a picture of λ in circle
picture_of_lambda() ->
  new_svg_picture([
    new_svg_line(0.1, 0.1, 0.35, 0.15),
    new_svg_line(0.35, 0.15, 0.45, 0.25),
    new_svg_line(0.45, 0.25, 0.9, 0.9),
    new_svg_line(0.1, 0.9, 0.5, 0.35)
    % new_svg_ellipse(new_vector(0.5, 0.5), 0.6, 0.6)
  ]).


% This is a picture of \
picture_of_slash() ->
  new_svg_picture([new_svg_line(0, 0, 1, 1)]).


% Means of combination in Henderson-Escher language


beside(Left, Right) ->
  fun (Frame) ->
    NewHorizontal = scale_vector(frame_horiz(Frame), 0.5),
    Origin = frame_origin(Frame),
    Vertical = frame_vert(Frame),
    Left(new_frame(Origin, NewHorizontal, Vertical)) ++
    Right(new_frame(add_vector(Origin, NewHorizontal),
                    NewHorizontal,
                    Vertical))
  end.


above(Top, Bottom) ->
  fun (Frame) ->
    NewVertical = scale_vector(frame_vert(Frame), 0.5),
    Origin = frame_origin(Frame),
    Horizontal = frame_horiz(Frame),
    Top(new_frame(Origin, Horizontal, NewVertical)) ++
    Bottom(new_frame(add_vector(Origin, NewVertical),
                     Horizontal,
                     NewVertical))
  end.


flip_vert(Picture) ->
  fun (Frame) ->
    Vertical = frame_vert(Frame),
    Picture(new_frame(add_vector(frame_origin(Frame), Vertical),
                      frame_horiz(Frame),
                      invert_vector(Vertical)))
  end.


flip_horiz(Picture) ->
  fun (Frame) ->
    Horizontal = frame_horiz(Frame),
    Picture(new_frame(add_vector(frame_origin(Frame), Horizontal),
                      invert_vector(Horizontal),
                      frame_vert(Frame)))
  end.


square_of_four(Picture) ->
  Top = beside(flip_horiz(Picture), Picture),
  above(Top, flip_vert(Top)).


simple_right_split(Picture, 0) ->
  Picture;
simple_right_split(Picture, N) ->
  Smaller = simple_right_split(Picture, N - 1),
  beside(Picture, above(Smaller, Smaller)).


-type compose(A) :: fun((picture(A), picture(A)) -> picture(A)).
-spec split(compose(A), compose(A)) ->
  fun((picture(A), integer()) -> picture(A)).


split(Compose1, Compose2) ->
  fun (Picture, N) ->
    lists:foldl(fun (_, Acc) ->
                  Compose1(Picture, Compose2(Acc, Acc))
                end,
                Picture,
                lists:seq(1, N))
  end.


flip(F) -> fun (B, A) -> F(A, B) end.


right_split(Picture, N) ->
  F = split(fun beside/2, fun above/2),
  F(Picture, N).


up_split(Picture, N) ->
  F = split(flip(fun above/2), fun beside/2),
  F(Picture, N).


corner_split(Picture, 0) ->
  Picture;
corner_split(Picture, N) ->
  K = N - 1,
  Up = up_split(Picture, K),
  Right = right_split(Picture, K),
  Corner = corner_split(Picture, K),
  beside(above(Up, Picture),
         above(Corner, Right)).


square_limit(Picture, N) ->
  square_of_four(corner_split(Picture, N)).


-type transform(A) :: fun((picture(A)) -> picture(A)).
-spec square_of_four_transforms(transform(A), transform(A), transform(A), transform(A)) -> transform(A).


square_of_four_transforms(UpLeft, UpRight, DownLeft, DownRight) ->
  fun (Picture) ->
    above(beside(UpLeft(Picture), UpRight(Picture)),
          beside(DownLeft(Picture), DownRight(Picture)))
  end.


-spec square_limit_transform(integer()) -> fun((picture(A)) -> picture(A)).


square_limit_transform(N) ->
  fun (Picture) ->
    square_limit(Picture, N)
  end.


% Main entry point of an Erlang script that can be run via escript
main([]) ->
  Picture = picture_of_lambda(),
  Frame = new_frame(new_vector(0, 0),
                    new_vector(200, 0),
                    new_vector(0, 200)),
  Result = square_limit(Picture, 5),
  io:format("~s~n", [picture_to_svg(Result, Frame, "10cm", "10cm")]).


% Some unit tests


vector_test() ->
  ?assertEqual(1, x_coord(new_vector(1, 2))).


vector_arithmetic_test() ->
  ?assertEqual(new_vector(2.0, 1.0),
               add_vector(new_vector(1.0, 0.0),
                          new_vector(1.0, 1.0))),
  ?assertEqual(new_vector(-0.5, 0.0),
               subtract_vector(new_vector(0.0, 1.0),
                               new_vector(0.5, 1.0))),
  ?assertEqual(new_vector(0.5, 0.5),
               scale_vector(new_vector(1.0, 1.0), 0.5)).


map_coord_test() ->
  Frame = new_frame(new_vector(2, 0),
                    new_vector(0, 2),
                    new_vector(-2, 0)),
  ?assertEqual(new_vector(0, 2),
               map_vector(Frame, new_vector(1, 1))).


simple_picture_test() ->
  Slash = picture_of_slash(),
  ?assertEqual([new_svg_line(0, 0, 1, 1)], Slash(identity_frame())).


beside_test() ->
  Slash = picture_of_slash(),
  Picture = beside(Slash, Slash),
  ?assertEqual([
                 new_svg_line(0.0, 0.0, 0.5, 1.0),
                 new_svg_line(0.5, 0.0, 1.0, 1.0)
               ],
               Picture(identity_frame())).


map_to_frame_test() ->
  Frame = new_frame(new_vector(0, 0),
                    new_vector(2, 0),
                    new_vector(0, 2)),
  ?assertEqual(new_svg_line(0.0, 0.0, 2.0, 2.0),
               map_to_frame(new_svg_line(0.0, 0.0, 1.0, 1.0), Frame)),
  ?assertEqual(new_svg_ellipse(new_vector(0.2, 0.2), 0.2, 0.2),
               map_to_frame(new_svg_ellipse(new_vector(0.1, 0.1), 0.1, 0.1), Frame)).
