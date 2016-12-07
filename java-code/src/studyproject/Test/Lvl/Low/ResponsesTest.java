package studyproject.Test.Lvl.Low;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.junit.Test;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Low.Responses;

public class ResponsesTest {

	@Test
	public void testRespondInfo() {
		System.out.println("*** testRespondInfo");
		BufferedOutputStream socketStream;
		long timestamp = System.currentTimeMillis();
		ArrayList<FileInfo> fileInfos = new ArrayList<FileInfo>();
		BufferedReader inputStream;
		String readWritePath = "testData/Lvl.Low/readWriteFile.txt";

		try {
			socketStream = new BufferedOutputStream(new FileOutputStream(
					readWritePath));
			inputStream = new BufferedReader(new FileReader(readWritePath));
			fileInfos.add(new FileInfo("000001", 1000001, "asdf1.txt", "",
					FileAction.add));
			fileInfos.add(new FileInfo("000002", 1000002, "asdf2.txt", "",
					FileAction.del));
			if (Responses.respondInfo(socketStream, 0, fileInfos) == 0) {
				assertEquals("all 0 2", inputStream.readLine());
				assertEquals("add 000001 1000001 asdf1.txt",
						inputStream.readLine());
				assertEquals("del 000002 1000002 asdf2.txt",
						inputStream.readLine());
			} else {
				System.out.println("respondInfo failed");
			}
			socketStream.close();
			inputStream.close();

			fileInfos.clear();
			Files.deleteIfExists(Paths.get(readWritePath));
			Files.createFile(Paths.get(readWritePath));
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}

		try {
			socketStream = new BufferedOutputStream(new FileOutputStream(
					readWritePath));
			inputStream = new BufferedReader(new FileReader(readWritePath));
			fileInfos.add(new FileInfo("000003", 1000003, "FileWithoutDot", "",
					FileAction.add));
			if (Responses.respondInfo(socketStream, timestamp, fileInfos) == 0) {
				assertEquals("upd " + timestamp + " 1", inputStream.readLine());
				assertEquals("add 000003 1000003 FileWithoutDot",
						inputStream.readLine());
			} else {
				System.out.println("respondInfo failed");
			}
			socketStream.close();
			inputStream.close();

			fileInfos.clear();
			Files.deleteIfExists(Paths.get(readWritePath));
			Files.createFile(Paths.get(readWritePath));
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

	@Test
	public void testRespondFile() {
		System.out.println("*** testRespondFile");
		BufferedOutputStream socketStream;
		BufferedReader inputStream;
		String readWritePath = "testData/Lvl.Low/readWriteFile.txt";
		String readPath = "testData/Lvl.Low/readFile.txt";

		try {
			socketStream = new BufferedOutputStream(new FileOutputStream(
					readWritePath));
			inputStream = new BufferedReader(new FileReader(readWritePath));
			FileInputStream fileInStream = new FileInputStream(readPath);
			if (Responses.respondFile(socketStream, fileInStream, 0, 100) == 0) {
				assertEquals(
						"Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
								+ "sed diam nonumy eirmod tempor invidunt ut l",
						inputStream.readLine());
			}
			fileInStream.close();
			inputStream.close();
			socketStream.close();
			Files.deleteIfExists(Paths.get(readWritePath));
			Files.createFile(Paths.get(readWritePath));
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

	@Test
	public void testRespondSendPermission() {
		System.out.println("*** testRespondSendPermission");
		String readWritePath = "testData/Lvl.Low/readWriteFile.txt";
		String writePath = "testData/Lvl.Low/writeFile.txt";
		String readPath = "testData/Lvl.Low/readFile.txt";

		BufferedReader readWriteInStream;
		BufferedReader writeInStream;

		try {
			SocketInheritor sockInheri = new SocketInheritor(readWritePath,
					readPath);
			FileOutputStream fileOutput = new FileOutputStream(writePath);
			if (Responses.respondSendPermission(sockInheri, fileOutput, 100) == 0) {
				fileOutput.close();
				readWriteInStream = new BufferedReader(new FileReader(
						readWritePath));
				writeInStream = new BufferedReader(new FileReader(writePath));

				assertEquals("OK", readWriteInStream.readLine());
				assertEquals(
						"Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
								+ "sed diam nonumy eirmod tempor invidunt ut l",
						writeInStream.readLine());

				readWriteInStream.close();
				writeInStream.close();

				Files.deleteIfExists(Paths.get(readWritePath));
				Files.createFile(Paths.get(readWritePath));

				Files.deleteIfExists(Paths.get(writePath));
				Files.createFile(Paths.get(writePath));
			} else {
				fileOutput.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

}
