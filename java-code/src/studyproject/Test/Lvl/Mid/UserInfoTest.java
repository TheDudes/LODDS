package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.*;

import org.junit.Test;

import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UserInfoTest {

	@Test
	public void testStripUsernameWithInvalidChars() {
		String invalid = "a b c d ; e @";
		String stripped = UserInfo.stripInvalidUsernameChars(invalid);
		assertEquals("abcde", stripped);
	}
	
	@Test
	public void testValidUsernames() {
		assert(UserInfo.validateUserName("k"));
		assert(UserInfo.validateUserName("k123"));
		assert(UserInfo.validateUserName("abcdefghijklmnopqrstuvwxyz12345"));
		assertFalse(UserInfo.validateUserName("@ invalid"));

	}
	
	@Test
	public void testStripTooLongUsername() {
		String invalid = "111111111111111111111111111111112222";
		String stripped = UserInfo.stripInvalidUsernameChars(invalid);
		assertEquals("11111111111111111111111111111111", stripped);
	}

}
