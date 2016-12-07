package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Mid.SendFileWPThread;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.Test.FileUtil;

/**
 * Junit tests for the SendFileWPThread class
 * 
 * @author Michael
 *
 */
public class SendFileWPTest {

	private String ip = "127.0.0.1";
	private int port = 9002;
	private String userName = "JunitTest";
	private long timeout = 5000;

	/**
	 * test functioning connection
	 */
	@Test
	public void testSendFileWP() {
		FileUtil.createFile(FileUtil.fileContents);

		try {
			File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);
			String originHash = FileHasher.getFileHash(FileUtil.dir + "/"
					+ FileUtil.sourceFile);
			FileInfo fileInfo = new FileInfo(originHash, file.length(),
					FileUtil.sourceFile, "", null);
			fileInfo.parentDirectory = file.getParent();

			UserInfo user = new UserInfo(InetAddress.getByName(ip), port,
					userName, 0, 0, null, null);

			SendFileWPTestClient testClient = new SendFileWPTestClient(
					InetAddress.getByName(ip), port, FileUtil.dir + "/"
							+ FileUtil.destFile, file.length(), 0);
			FileUtil.addToCleanUp(FileUtil.destFile);
			testClient.start();

			SendFileWPThread sendFileWP = new SendFileWPThread(user, timeout,
					fileInfo);
			sendFileWP.start();

			testClient.join();
			sendFileWP.join();

			assertEquals(
					FileHasher.getFileHash(FileUtil.dir + "/"
							+ FileUtil.destFile), originHash);

		} catch (IOException | NoSuchAlgorithmException | InterruptedException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

	/**
	 * test nonresponding other client
	 */
	@Test
	public void testSendFileWPTimeout() {
		FileUtil.createFile(FileUtil.fileContents);

		try {
			File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);
			String originHash = FileHasher.getFileHash(FileUtil.dir + "/"
					+ FileUtil.sourceFile);
			FileInfo fileInfo = new FileInfo(originHash, file.length(),
					FileUtil.sourceFile, "", null);
			fileInfo.parentDirectory = file.getParent();

			UserInfo user = new UserInfo(InetAddress.getByName(ip), port,
					userName, 0, 0, null, null);

			SendFileWPTestClient testClient = new SendFileWPTestClient(
					InetAddress.getByName(ip), port, FileUtil.dir + "/"
							+ FileUtil.destFile, file.length(), timeout + 1000);
			FileUtil.addToCleanUp(FileUtil.destFile);
			testClient.start();

			SendFileWPThread sendFileWP = new SendFileWPThread(user, timeout,
					fileInfo);
			sendFileWP.start();

			sendFileWP.join();

			Thread.sleep(500);
			assertTrue(testClient.isAlive());

			testClient.interrupt();

		} catch (IOException | NoSuchAlgorithmException | InterruptedException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

}
