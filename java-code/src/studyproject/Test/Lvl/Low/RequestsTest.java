package studyproject.Test.Lvl.Low;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

import studyproject.API.Lvl.Low.Requests;
import studyproject.Test.FileUtil;

/**
 * class to test functionality of the Requests class
 * 
 * @author Michael
 *
 */
public class RequestsTest {

	private String filePath = "testFile";
	private String checksum = "96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b";
	private String infoMessage = "get info 2189391831";
	private String permissionMessage = "get send-permission 3898324 5000 testFile";
	private String fileMessage = "get file 96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b 0 90000";
	private long timeStamp = 2189391831L;
	private long fileSize = 3898324L;
	private long timeout = 5000L;
	private long endIndex = 90000;

	/**
	 * check if the message sent by the getInfo method is as specified
	 */
	@Test
	public void testGetInfo() {
		FileUtil.createFile("", filePath);
		try (BufferedOutputStream socketStream = new BufferedOutputStream(
				new FileOutputStream(FileUtil.dir + "/" + filePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + filePath))) {
			Requests.getInfo(socketStream, timeStamp);

			assertEquals(infoMessage, inputStream.readLine());
		} catch (IOException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

	/**
	 * check if the message sent by the getSendPermission method is as specified
	 */
	@Test
	public void testGetSendPermission() {
		FileUtil.createFile("", filePath);
		try (BufferedOutputStream socketStream = new BufferedOutputStream(
				new FileOutputStream(FileUtil.dir + "/" + filePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + filePath))) {
			Requests.getSendPermission(socketStream, fileSize, timeout, filePath);

			assertEquals(permissionMessage, inputStream.readLine());
		} catch (IOException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

	/**
	 * check if the message sent by the getFile method is as specified
	 */
	@Test
	public void testGetFile() {
		FileUtil.createFile("", filePath);
		try (BufferedOutputStream socketStream = new BufferedOutputStream(
				new FileOutputStream(FileUtil.dir + "/" + filePath));
				BufferedReader inputStream = new BufferedReader(new FileReader(FileUtil.dir + "/" + filePath))) {
			Requests.getFile(socketStream, checksum, 0, endIndex);

			assertEquals(fileMessage, inputStream.readLine());
		} catch (IOException e) {
			e.printStackTrace();
			FileUtil.cleanUp();
			fail();
		}
		FileUtil.cleanUp();
	}

}
