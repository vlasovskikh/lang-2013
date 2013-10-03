import junit.framework.TestCase;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

/**
 * @author vlan
 */
public class IpAddressUtilTest extends TestCase {
  @Test
  public void testSimpleIpV4() {
    final List<Integer> ip = Arrays.asList(127, 0, 0, 1);
    String actual = IpAddressUtil.ipV4ToString1(ip);
    assertEquals("127.0.0.1", actual);
  }

  @Test
  public void testSimpleIpV6() {
    final List<Integer> ip = Arrays.asList(0x2001, 0x0DB8, 0xAC10, 0xFE01, 0, 0, 0, 0);
    final String actual = IpAddressUtil.ipV6ToString2(ip);
    assertEquals("2001:db8:ac10:fe01:0:0:0:0", actual);
  }

  // Do we need more tests here?
}
