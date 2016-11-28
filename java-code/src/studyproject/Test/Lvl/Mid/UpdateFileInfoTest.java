package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
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

public class UpdateFileInfoTest {
	
	@Test
	public void testUpdateFileInfoThread(){
		String localhost = "127.0.0.1";
		int port = 9002;
		String testChecksum1 = "96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b";
		String testFile1 = "testFile1";
		String testChecksum2 = "1d98362d093cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31";
		String testFile2 = "testFile2";
		Vector<String> fileNames = new Vector<String>();
		fileNames.add(testFile1);
		ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
		ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
		pathToFileInfo.put(testFile1, new FileCoreInfo(testChecksum1, 123123));
		checksumToPath.put(testChecksum1, fileNames);
		String message = "upd 1464269498 2\n"
				+ "del " + testChecksum1 + " 421341231 " + testFile1 + "\n"
				+ "add " + testChecksum2 + " 421341231 " + testFile2 + "\n";

		try {
			UpdateFileInfoTestServer testFileInfoTestServer = new UpdateFileInfoTestServer(InetAddress.getByName(localhost), port, message);
			testFileInfoTestServer.start();
			UserInfo userInfo = new UserInfo(InetAddress.getByName(localhost), port, "testUser", 0L, 0L, pathToFileInfo, checksumToPath, 0L);
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.start();

			while(updater.isAlive() | testFileInfoTestServer.isAlive()){
				Thread.sleep(500);
			}

			assertEquals(userInfo.getPathToFileInfo().size(), 1);
			assertTrue(userInfo.getPathToFileInfo().containsKey(testFile2));
			assertEquals(userInfo.getPathToFileInfo().get(testFile2).getChecksum(), testChecksum2);
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

}
