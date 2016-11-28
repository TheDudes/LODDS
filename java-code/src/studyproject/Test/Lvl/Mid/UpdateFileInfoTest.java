package studyproject.Test.Lvl.Mid;

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
		String testChecksum = "asdasad1234asd124ad1232143sdf";
		String testFile = "testFile1";
		Vector<String> fileNames = new Vector<String>();
		fileNames.add(testFile);
		ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
		ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
		pathToFileInfo.put(testFile, new FileCoreInfo(testChecksum, 123123));
		checksumToPath.put(testChecksum, fileNames);

		try {
			UserInfo userInfo = new UserInfo(InetAddress.getByName("127.0.0.1"), 9002, "testUser", 0L, 0L, pathToFileInfo, checksumToPath, 0L);
			UpdateFileInfoThread updater = new UpdateFileInfoThread(userInfo);
			updater.run();
		} catch (UnknownHostException e) {
			e.printStackTrace();
			fail();
		}
	}

}
