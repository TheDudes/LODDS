package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.NoSuchAlgorithmException;

import org.junit.Test;

import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Mid.FileSenderThread;

public class FileSenderTest {

	private String ip = "127.0.0.1";
	private int port = 9002;
	private int endIndex = 200;
	private int startIndex = 201;
	private String dir = "test";
	private String sourceFile = "sourceFile";
	private String destFile = "destFile";
	private String fileContents = "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed"
			+ " eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim"
			+ " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi"
			+ " consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu"
			+ " fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in"
			+ " culpa qui officia deserunt mollit anim id est laborum.";

	/**
	 * creates a file and then transmits it via the FileSenderThread and the
	 * FileSenderTestClient. Finally checks if the transmitted file is the same
	 * as the original file and then deletes the files again
	 */
	@Test
	public void testSendFile() {
		String originHash;
		createFile();
		File file = new File(dir + "/" + sourceFile);

		try {
			originHash = FileHasher.getFileHash(dir + "/" + sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, dir + "/" + destFile, file.length());
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(), sourceFile, null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket, fileInfo);
			fileSender.start();

			testClient.join();
			fileSender.join();

			assertEquals(FileHasher.getFileHash(dir + "/" + destFile),
					originHash);

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			cleanUp();
			fail();
		}
		cleanUp();
	}

	/**
	 * creates a file and then transmits a part of it via the FileSenderThread and the
	 * FileSenderTestClient. Finally checks if the part that was transmitted is the same
	 * as the original part.
	 * Deletes the files after they are no longer used
	 */
	@Test
	public void testSendFileFirstPart() {
		String originHash;
		createFile();
		File file = new File(dir + "/" + sourceFile);

		try {
			originHash = FileHasher.getFileHash(dir + "/" + sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, dir + "/" + destFile, endIndex);
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(), sourceFile, null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket, fileInfo, 0, endIndex);
			fileSender.start();

			testClient.join();
			fileSender.join();

			FileReader fileReader = new FileReader(new File(dir + "/" + destFile));
			char[] buffer = new char[endIndex];
			fileReader.read(buffer, 0, endIndex);
			fileReader.close();

			assertEquals(String.valueOf(buffer), fileContents.substring(0, endIndex));

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			cleanUp();
			fail();
		}
		cleanUp();
	}


	/**
	 * creates a file and then transmits a part of it via the FileSenderThread and the
	 * FileSenderTestClient. Finally checks if the part that was transmitted is the same
	 * as the original part.
	 * Deletes the files after they are no longer used
	 */
	@Test
	public void testSendFileLastPart() {
		String originHash;
		createFile();
		File file = new File(dir + "/" + sourceFile);

		try {
			originHash = FileHasher.getFileHash(dir + "/" + sourceFile);

			FileSenderTestClient testClient = new FileSenderTestClient(
					InetAddress.getByName(ip), port, dir + "/" + destFile, file.length() - startIndex);
			testClient.start();

			FileInfo fileInfo = new FileInfo(originHash, file.length(), sourceFile, null);
			fileInfo.parentDirectory = file.getParent();

			Socket socket = new Socket(InetAddress.getByName(ip), port);

			FileSenderThread fileSender = new FileSenderThread(socket, fileInfo, startIndex, 0);
			fileSender.start();

			testClient.join();
			fileSender.join();

			FileReader fileReader = new FileReader(new File(dir + "/" + destFile));
			char[] buffer = new char[(int)(file.length() - startIndex)];
			fileReader.read(buffer, 0, (int)(file.length() - startIndex));
			fileReader.close();

			assertEquals(String.valueOf(buffer), fileContents.substring(startIndex, fileContents.length()));

		} catch (NoSuchAlgorithmException | IOException | InterruptedException e) {
			e.printStackTrace();
			cleanUp();
			fail();
		}
		cleanUp();
	}

	/**
	 * create a file for testing
	 */
	private void createFile() {
		File file = new File(dir);
		file.mkdir();
		try (BufferedWriter writer = new BufferedWriter(new FileWriter(dir
				+ "/" + sourceFile))) {
			writer.write(fileContents);
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * delete the created files
	 */
	private void cleanUp(){
		File file = new File(dir + "/" + sourceFile);
		if(!file.delete()){
			System.err.println("could not delete file " + sourceFile);
		}
		file = new File(dir + "/" + destFile);
		if(!file.delete()){
			System.err.println("could not delete file " + destFile);
		}
		file = new File(dir);
		if(!file.delete()){
			System.err.println("could not delete directory " + dir);
		}
	}

}
