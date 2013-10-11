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


% Segments are the only vector graphic elements available at the
% moment
-type segment() :: {segment, vector(), vector()}.

-spec new_segment(Start :: vector(), End :: vector()) -> segment().
-spec new_segment(X1 :: number(), Y1 :: number(),
                  X2 :: number(), Y2 :: number()) -> segment().
-spec segment_start(segment()) -> vector().
-spec segment_end(segment()) -> vector().


% SVG is used here only in the serialized string form
-type svg() :: string().


% Segments are SVG primitives
-spec segment_to_svg(segment()) -> svg().


% Picture is a function that when given a frame, returns a list of
% some elements representing the picture in this frame. Picture
% is the only primitive in Henderson-Escher language
-type picture(A) :: fun((frame()) -> [A]).


% Currently we deal only with pictures that consists of segments
-spec new_segmented_picture([segment()]) -> picture(segment()).
-spec picture_to_svg(picture(segment()), frame(),
                     Width :: string(),
                     Height :: string()) -> svg().

% Henderson-Escher language of pictures has some means of
% combination
-spec beside(picture(A), picture(A)) -> picture(A).
-spec above(picture(A), picture(A)) -> picture(A).
-spec flip_vert(picture(A)) -> picture(A).
-spec flip_horiz(picture(A)) -> picture(A).


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


% Constructors and selectors of segment()
new_segment(Start, End) -> {segment, Start, End}.
new_segment(X1, Y1, X2, Y2) ->
  new_segment(new_vector(X1, Y1),
              new_vector(X2, Y2)).
segment_start({segment, Start, _}) -> Start.
segment_end({segment, _, End}) -> End.


% Segment is a SVG (Scalable Vector Graphics) primitive, so this is how
% a segment becomes SVG data
segment_to_svg(Segment) ->
  Start = segment_start(Segment),
  End = segment_end(Segment),
  io_lib:format("<line x1='~w' y1='~w' x2='~w' y2='~w' stroke-width='5'/>",
                [
                  x_coord(Start),
                  y_coord(Start),
                  x_coord(End),
                  y_coord(End)
                ]).

% Creating a picture from a list of segments means creating a function
% that when given a frame, vector-maps all the segments to this frame
new_segmented_picture(Segments) ->
  fun (Frame) ->
    lists:map(fun (Segment) ->
                new_segment(map_vector(Frame, segment_start(Segment)),
                            map_vector(Frame, segment_end(Segment)))
              end,
              Segments)
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
  Segments = Picture(Frame),
  Body = string:join(lists:map(fun segment_to_svg/1, Segments),
                     "\n"),
  Footer =
    "\n"
    "  </g>\n"
    "</svg>",
  Header ++ Body ++ Footer.


% This is a picture of λ
picture_of_lambda() ->
  new_segmented_picture([
    new_segment(0.1, 0.1, 0.35, 0.15),
    new_segment(0.35, 0.15, 0.45, 0.25),
    new_segment(0.45, 0.25, 0.9, 0.9),
    new_segment(0.1, 0.9, 0.5, 0.35)
  ]).


% This is a picture of \
picture_of_slash() ->
  new_segmented_picture([new_segment(0, 0, 1, 1)]).


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


% Main entry point of an Erlang script that can be run via escript
main([]) ->
  Picture = picture_of_lambda(),
  Frame = new_frame(new_vector(0, 0),
                    new_vector(200, 0),
                    new_vector(0, 200)),
  Top = beside(flip_horiz(Picture), Picture),
  Result = above(Top, flip_vert(Top)),
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
  ?assertEqual([new_segment(0, 0, 1, 1)], Slash(identity_frame())).


beside_test() ->
  Slash = picture_of_slash(),
  Picture = beside(Slash, Slash),
  ?assertEqual([
                 new_segment(0.0, 0.0, 0.5, 1.0),
                 new_segment(0.5, 0.0, 1.0, 1.0)
               ],
               Picture(identity_frame())).
