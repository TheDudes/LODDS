package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.Test;

import studyproject.API.Lvl.Mid.UpdateFileInfoThread;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * Junit tests for the UpdateFileInfoThread
 * 
 * @author Michael
 *
 */
public class UpdateFileInfoTest {

	private String localhost = "127.0.0.1";
	private int port = 9002;
	private String testChecksum1 = "96fa8f226d3801741e807533552bc4b177ac4544";
	private String testFile1 = "/testFile1";
	private String testChecksum2 = "1d98362d093cb271d4f03f89b4af93996b3bf43a";
	private String testFile2 = "/testFile2";

	/**
	 * send a message with an add, there should be both files in the hashmap
	 * afterwards
	 */
	@Test
	public void testUpdateFileInfoThreadUpdAdd() {
		String message = "upd 1464269498 1\n" + "add " + testChecksum2
				+ " 421341231 " + testFile2 + "\n";

		try {
			UpdateFileInfoTestServer testFileInfoTestServer = new UpdateFileInfoTestServer(
					InetAddress.getByName(localhost), port, message);
			testFileInfoTestServer.start();
			UserInfo userInfo = createUser();
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.start();

			while (updater.isAlive() | testFileInfoTestServer.isAlive()) {
				Thread.sleep(500);
			}

			assertEquals(userInfo.getPathToFileInfo().size(), 2);
			assertTrue(userInfo.getPathToFileInfo().containsKey(testFile2));
			assertEquals(userInfo.getPathToFileInfo().get(testFile2)
					.getChecksum(), testChecksum2);
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * send a delete message, there should be no files left in the hashmap
	 * afterwards
	 */
	@Test
	public void testUpdateFileInfoThreadUpdDel() {
		String message = "upd 1464269498 1\n" + "del " + testChecksum1
				+ " 421341231 " + testFile1 + "\n";

		try {
			UpdateFileInfoTestServer testFileInfoTestServer = new UpdateFileInfoTestServer(
					InetAddress.getByName(localhost), port, message);
			testFileInfoTestServer.start();
			UserInfo userInfo = createUser();
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.start();

			while (updater.isAlive() | testFileInfoTestServer.isAlive()) {
				Thread.sleep(500);
			}

			assertEquals(userInfo.getPathToFileInfo().size(), 0);
			assertFalse(userInfo.getPathToFileInfo().containsKey(testFile2));
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * send a mix of add and delete messages
	 */
	@Test
	public void testUpdateFileInfoThreadUpdDelAdd() {
		String message = "all 1464269498 2\n" + "del " + testChecksum1
				+ " 421341231 " + testFile1 + "\n" + "add " + testChecksum2
				+ " 2234323 " + testFile2 + "\n";

		try {
			UpdateFileInfoTestServer testFileInfoTestServer = new UpdateFileInfoTestServer(
					InetAddress.getByName(localhost), port, message);
			testFileInfoTestServer.start();
			UserInfo userInfo = createUser();
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.start();

			while (updater.isAlive() | testFileInfoTestServer.isAlive()) {
				Thread.sleep(500);
			}

			assertEquals(userInfo.getPathToFileInfo().size(), 1);
			assertFalse(userInfo.getPathToFileInfo().containsKey(testFile1));
			assertTrue(userInfo.getPathToFileInfo().containsKey(testFile2));
			assertEquals(userInfo.getChecksumToPath().get(testChecksum2)
					.firstElement(), testFile2);
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * send an update all signal with 2 files, thus the 2 files from the message
	 * should be in the hashmaps afterwards
	 */
	@Test
	public void testUpdateFileInfoThreadAll() {
		String message = "all 1464269498 2\n" + "add " + testChecksum1
				+ " 421341231 " + testFile1 + "\n" + "add " + testChecksum2
				+ " 2234323 " + testFile2 + "\n";

		try {
			UpdateFileInfoTestServer testFileInfoTestServer = new UpdateFileInfoTestServer(
					InetAddress.getByName(localhost), port, message);
			testFileInfoTestServer.start();
			UserInfo userInfo = createUser();
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.start();

			while (updater.isAlive() | testFileInfoTestServer.isAlive()) {
				Thread.sleep(500);
			}

			assertEquals(userInfo.getPathToFileInfo().size(), 2);
			assertTrue(userInfo.getPathToFileInfo().containsKey(testFile1));
			assertTrue(userInfo.getPathToFileInfo().containsKey(testFile2));
			assertEquals(userInfo.getChecksumToPath().get(testChecksum1)
					.firstElement(), testFile1);
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

	private UserInfo createUser() throws UnknownHostException {
		Vector<String> fileNames = new Vector<String>();
		fileNames.add(testFile1);
		ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
		ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
		pathToFileInfo.put(testFile1, new FileCoreInfo(testChecksum1, 123123,
				testFile1));
		checksumToPath.put(testChecksum1, fileNames);
		UserInfo userInfo = new UserInfo(InetAddress.getByName(localhost),
				port, "testUser", 0L, 0L, pathToFileInfo, checksumToPath, 0L);
		return userInfo;
	}

}
