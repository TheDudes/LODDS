package studyproject.Test.Lvl.Low;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Lvl.Low.Handles;
import studyproject.Test.FileUtil;

/**
 * class for junit tests for the Handles class
 * 
 * @author Michael
 *
 */
public class HandlesTest {
	
	private static String readFrom = "readFromFile";
	private String filePath = "testFile";
	private String filePath2 = "testFile2";
	private String filePath3 = "testFile3";
	private String message = "upd 1464269498 6\n"
			  + "del bf43aae6294fef31d4f03f8a8f226d380b3bf43aae6294fef3bc4e4b61234f31 23411123 /some-file.txt\n"
		      + "add 1d98362d093cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 421341231 /some-file.txt\n"
		      + "del 96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b 99344121 /some/directory/file.pdf\n"
		      + "del b42e80cfd93cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 843418123 /some/other/directory/foo.bar\n"
		      + "add a29b6e31593cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 123123 /bar.foo\n"
		      + "add 28ad16dcd93cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 3214178546 /new-file.zip\n";
	private String messageAll = "all 1464269498 5\n"
		      + "add 1d98362d093cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 421341231 /some-file.txt\n"
		      + "add 96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b 99344121 /some/directory/file.pdf\n"
		      + "add b42e80cfd93cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 843418123 /some/other/directory/foo.bar\n"
		      + "add a29b6e31593cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 123123 /bar.foo\n"
		      + "add 28ad16dcd93cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31 3214178546 /new-file.zip\n";
	
	/**
	 * create a file for the testHandleFile method to read from
	 */
	@BeforeClass
	public static void setUp(){
		FileUtil.createFile(FileUtil.fileContents, readFrom);
	}
	
	/**
	 * delete all files created in the tests
	 */
	@AfterClass
	public static void cleanUp(){
		FileUtil.cleanUp();
	}
	
	/**
	 * tests if the handleFile method writes the data from the socket to the file
	 * in the correct manner
	 */
	@Test
	public void testHandleFile() {
		FileUtil.createFile("", filePath);
		try (BufferedInputStream socketStream = new BufferedInputStream(
				new FileInputStream(FileUtil.dir + "/" + readFrom));
				FileOutputStream fileStream = new FileOutputStream(FileUtil.dir + "/" + filePath);
				BufferedReader reader = new BufferedReader(new FileReader(FileUtil.dir + "/" + filePath))) {
			File readFromFile = new File(FileUtil.dir + "/" + readFrom);
			Handles.handleFile(socketStream, fileStream, readFromFile.length());

			assertEquals(FileUtil.fileContents, reader.readLine());
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}
	
	/**
	 * tests if the handleFileInfo method retrieves the information about the
	 * files from the socketstream in the correct way
	 * This tests the upd type of fileInfo message
	 */
	@Test
	public void testHandleInfo(){
		FileUtil.createFile(message, filePath2);
		ArrayList<FileInfo> fileInfos = new ArrayList<>();
		Timestamp timestamp = new Timestamp();
		FileInfoListType infoType = new FileInfoListType();
		try (BufferedReader socketStream = new BufferedReader(
				new FileReader(FileUtil.dir + "/" + filePath2))){
			Handles.handleInfo(socketStream, fileInfos, timestamp, infoType);
			
			assertEquals(1464269498L, timestamp.value);
			assertEquals(InfoType.upd, infoType.type);
			assertEquals(6, fileInfos.size());
			assertEquals("bf43aae6294fef31d4f03f8a8f226d380b3bf43aae6294fef3bc4e4b61234f31", fileInfos.get(0).checksum);
			assertEquals("/new-file.zip", fileInfos.get(5).fileName);
			assertEquals(843418123L, fileInfos.get(3).size);
			
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}
	
	/**
	 * tests if the handleFileInfo method retrieves the information about the
	 * files from the socketstream in the correct way
	 * This tests the all type of fileInfo message
	 */
	@Test
	public void testHandleInfoAll(){
		FileUtil.createFile(messageAll, filePath3);
		ArrayList<FileInfo> fileInfos = new ArrayList<>();
		Timestamp timestamp = new Timestamp();
		FileInfoListType infoType = new FileInfoListType();
		try (BufferedReader socketStream = new BufferedReader(
				new FileReader(FileUtil.dir + "/" + filePath3))){
			Handles.handleInfo(socketStream, fileInfos, timestamp, infoType);
			
			assertEquals(1464269498L, timestamp.value);
			assertEquals(InfoType.all, infoType.type);
			assertEquals(5, fileInfos.size());
			assertEquals("1d98362d093cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31", fileInfos.get(0).checksum);
			assertEquals("/new-file.zip", fileInfos.get(4).fileName);
			assertEquals(843418123L, fileInfos.get(2).size);
			
		} catch (IOException e) {
			e.printStackTrace();
			fail();
		}
	}

}
