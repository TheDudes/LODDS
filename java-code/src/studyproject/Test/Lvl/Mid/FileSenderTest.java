package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Mid.FileSenderThread;

/**
 * Junit tests for the FileSenderThread
 * 
 * @author Michael
 *
 */
public class FileSenderTest {

	private String ip = "127.0.0.1";
	private int port = 9002;
	private int endIndex = 200;
	private int startIndex = 201;

	/**
	 * creates a file and then transmits it via the FileSenderThread and the
	 * FileSenderTestClient. Finally checks if the transmitted file is the same
	 * as the original file and then deletes the files again
	 */
	@Test
	public void testSendFile() {
		String originHash;
		FileUtil.createFile();
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);

		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, FileUtil.dir + "/" + FileUtil.destFile,
					file.length());
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(),
					FileUtil.sourceFile, "", null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket, fileInfo);
			fileSender.start();

			testClient.join();
			fileSender.join();

			assertEquals(FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.destFile),
					originHash);

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

	/**
	 * creates a file and then transmits a part of it via the FileSenderThread
	 * and the FileSenderTestClient. Finally checks if the part that was
	 * transmitted is the same as the original part. Deletes the files after
	 * they are no longer used
	 */
	@Test
	public void testSendFileFirstPart() {
		String originHash;
		FileUtil.createFile();
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);

		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, FileUtil.dir + "/" + FileUtil.destFile,
					endIndex);
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(),
					FileUtil.sourceFile, "", null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket,
					fileInfo, 0, endIndex);
			fileSender.start();

			testClient.join();
			fileSender.join();

			FileReader fileReader = new FileReader(new File(FileUtil.dir + "/"
					+ FileUtil.destFile));
			char[] buffer = new char[endIndex];
			fileReader.read(buffer, 0, endIndex);
			fileReader.close();

			assertEquals(String.valueOf(buffer),
					FileUtil.fileContents.substring(0, endIndex));

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

	/**
	 * creates a file and then transmits a part of it via the FileSenderThread
	 * and the FileSenderTestClient. Finally checks if the part that was
	 * transmitted is the same as the original part. Deletes the files after
	 * they are no longer used
	 */
	@Test
	public void testSendFileLastPart() {
		String originHash;
		FileUtil.createFile();
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);

		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, FileUtil.dir + "/" + FileUtil.destFile,
					file.length() - startIndex);
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(),
					FileUtil.sourceFile, "", null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket,
					fileInfo, startIndex, 0);
			fileSender.start();

			testClient.join();
			fileSender.join();

			FileReader fileReader = new FileReader(new File(FileUtil.dir + "/"
					+ FileUtil.destFile));
			char[] buffer = new char[(int) (file.length() - startIndex)];
			fileReader.read(buffer, 0, (int) (file.length() - startIndex));
			fileReader.close();

			assertEquals(String.valueOf(buffer),
					FileUtil.fileContents.substring(startIndex,
							FileUtil.fileContents.length()));

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

}
