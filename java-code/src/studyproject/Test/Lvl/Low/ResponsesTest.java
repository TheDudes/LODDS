package studyproject.Test.Lvl.Low;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Low.Responses;
import studyproject.Test.FileUtil;

public class ResponsesTest {

	//private String readWritePath = "java-code/testData/Lvl.Low/readWriteFile.txt";
	//private String readPath = "java-code/testData/Lvl.Low/readFile.txt";
	//private String writePath = "java-code/testData/Lvl.Low/writeFile.txt";
	private String readWritePath = "readWriteFile.txt";
	private static String readPath = "readFile.txt";
	private String writePath = "writeFile.txt";
	
	 @BeforeClass 
	 public static void setUp() {
		 FileUtil.createFile(FileUtil.fileContents2, readPath);
	 }
	 
	 @AfterClass
	 public static void cleanUp(){
		 FileUtil.cleanUp();
	 }

	@Test
	public void testRespondInfo() throws IOException {
		System.out.println("*** testRespondInfo");
		long timestamp = System.currentTimeMillis();
		ArrayList<FileInfo> fileInfos = new ArrayList<FileInfo>();
		
		FileUtil.createFile("", readWritePath);

		try (BufferedOutputStream socketStream = new BufferedOutputStream(new FileOutputStream(FileUtil.dir + "/" + readWritePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + readWritePath))) {

			fileInfos.add(new FileInfo("000001", 1000001, "asdf1.txt", "", FileAction.add));
			fileInfos.add(new FileInfo("000002", 1000002, "asdf2.txt", "", FileAction.del));
			if (Responses.respondInfo(socketStream, 0, fileInfos) == 0) {
				assertEquals("all 0 2", inputStream.readLine());
				assertEquals("add 000001 1000001 asdf1.txt", inputStream.readLine());
				assertEquals("del 000002 1000002 asdf2.txt", inputStream.readLine());
			} else {
				System.out.println("respondInfo failed");
				fail();
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
		
		fileInfos.clear();
		FileUtil.deleteFile(readWritePath);
		FileUtil.createFile("", readWritePath);

		try (BufferedOutputStream socketStream = new BufferedOutputStream(new FileOutputStream(FileUtil.dir + "/" + readWritePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + readWritePath))) {
			fileInfos.add(new FileInfo("000003", 1000003, "FileWithoutDot", "", FileAction.add));
			if (Responses.respondInfo(socketStream, timestamp, fileInfos) == 0) {
				assertEquals("upd " + timestamp + " 1", inputStream.readLine());
				assertEquals("add 000003 1000003 FileWithoutDot", inputStream.readLine());
			} else {
				System.out.println("respondInfo failed");
				fail();
			}

		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
		
		fileInfos.clear();
		FileUtil.deleteFile(readWritePath);
	}

	@Test
	public void testRespondFile() throws IOException {
		System.out.println("*** testRespondFile");

		FileUtil.createFile("", readWritePath);
		
		try (BufferedOutputStream socketStream = new BufferedOutputStream(new FileOutputStream(FileUtil.dir + "/" + readWritePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + readWritePath));
				FileInputStream fileInStream = new FileInputStream(FileUtil.dir + "/" + readPath)){
			
			if (Responses.respondFile(socketStream, fileInStream, 0, 100) == 0) {
				assertEquals("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
						+ "sed diam nonumy eirmod tempor invidunt ut l", inputStream.readLine());
			}
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
		FileUtil.deleteFile(readWritePath);
	}

	@Test
	public void testRespondSendPermission() throws IOException {
		System.out.println("*** testRespondSendPermission");

		FileUtil.createFile("", readWritePath);
		FileUtil.createFile("", writePath);
		
		SocketInheritor sockInheri = new SocketInheritor(FileUtil.dir + "/" + readWritePath, FileUtil.dir + "/" + readPath);

		try (FileOutputStream fileOutput = new FileOutputStream(FileUtil.dir + "/" + writePath);
				BufferedReader readWriteInStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + readWritePath));
				BufferedReader writeInStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + writePath));){
			
			if (Responses.respondSendPermission(sockInheri, fileOutput, 100) == 0) {
				

				assertEquals("OK", readWriteInStream.readLine());
				assertEquals("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, "
						+ "sed diam nonumy eirmod tempor invidunt ut l", writeInStream.readLine());
				
				sockInheri.close();

			}
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
		
		FileUtil.deleteFile(readWritePath);
		FileUtil.deleteFile(writePath);
	}

}
