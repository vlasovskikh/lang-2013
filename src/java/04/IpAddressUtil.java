import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author vlan
 */
public class IpAddressUtil {
  public static String ipV4ToString1(@NotNull List<Integer> parts) {
    if (parts.size() != 4) {
      throw new IllegalArgumentException("IP list size must be 4");
    }
    String result = "";
    for (int i : parts) {
      result = result + Integer.toString(i) + ".";
    }
    return result.substring(0, result.length() - 1);
  }

  public static String ipV6ToString1(@NotNull List<Integer> parts) {
    if (parts.size() != 8) {
      throw new IllegalArgumentException("IP list size must be 8");
    }
    String result = "";
    for (int i : parts) {
      result = result + Integer.toHexString(i) + ":";
    }
    return result.substring(0, result.length() - 1);
  }

  @NotNull
  public static String join(@NotNull List<String> strings,
                            @NotNull String sep) {
    // Why StringBuilder is better than string concatenation?
    StringBuilder builder = new StringBuilder();
    boolean first = true;
    for (String s : strings) {
      if (first) {
        first = false;
      }
      else {
        builder.append(sep);
      }
      builder.append(s);
    }
    return builder.toString();
  }

  public static interface Function<A, B> {
    B invoke(A a);
  }

  @NotNull
  public static <A, B> List<B> map(
          @NotNull List<A> xs,
          @NotNull Function<A, B> f) {
    List<B> result = new ArrayList<B>();
    for (A x : xs) {
      result.add(f.invoke(x));
    }
    return result;
  }

  public static String ipV4ToString2(@NotNull List<Integer> parts) {
    // TODO: size = 4 x 8bit ints
    return join(map(parts,
                    new Function<Integer, String>() {
                      @Override
                      public String invoke(Integer i) {
                        return Integer.toString(i);
                      }
                    }),
                ".");
  }

  public static String ipV6ToString2(@NotNull List<Integer> parts) {
    // TODO: size = 8 x 16bit ints
    return join(map(parts,
                    new Function<Integer, String>() {
                      @Override
                      public String invoke(Integer i) {
                        return Integer.toHexString(i);
                      }
                    }),
                ":");
  }
}

