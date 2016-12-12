package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Files;
import java.security.NoSuchAlgorithmException;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import studyproject.API.Core.File.FileHasher;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.Test.FileUtil;

public class FileConnectionTest {

	private final String completeTest = "destComplete";
	private final String startTest = "destStart";
	private final String endTest = "destEnd";
	private String ip = "127.0.0.1";
	private int port = 9002;
	private int endIndex = 200;
	private int startIndex = 201;
	
	@BeforeClass
	public static void setUp(){
		FileUtil.createFile(FileUtil.fileContents2);
	}
	
	@AfterClass
	public static void cleanUp(){
		//FileUtil.cleanUp();
	}
	

	@Test
	public void testGetFileComplete() {
		String originHash;
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);
		FileUtil.createFile("", completeTest);

		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);
			FileConnectionTestServer testServer = new FileConnectionTestServer(InetAddress.getByName(ip), port,
					FileUtil.dir + "/" + FileUtil.sourceFile, 0L, file.length());
			testServer.start();

			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
			pathToFileInfo.put(FileUtil.dir + "/" + completeTest, new FileCoreInfo(originHash, file.length(), FileUtil.dir + "/" + completeTest));
			ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
			Vector<String> filePaths = new Vector<String>();
			filePaths.add(FileUtil.dir + "/" + completeTest);
			checksumToPath.put(originHash, filePaths);
			UserInfo userInfo = new UserInfo(InetAddress.getByName(ip), port, "test", 0L, 0L, pathToFileInfo, checksumToPath);
			FileConnectionThread fileThread = new FileConnectionThread(userInfo, originHash, file.length(),
					FileUtil.dir + "/" + completeTest);
			fileThread.start();
			
			testServer.join();
			fileThread.join();
			
			assertEquals(originHash, FileHasher.getFileHash(FileUtil.dir + "/" + completeTest));
			
		} catch (IOException | NoSuchAlgorithmException | InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}
	
	@Test
	public void testGetFileStart() {
		String originHash;
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);
		FileUtil.createFile("", startTest);

		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);
			FileConnectionTestServer testServer = new FileConnectionTestServer(InetAddress.getByName(ip), port,
					FileUtil.dir + "/" + FileUtil.sourceFile, 0L, endIndex);
			testServer.start();

			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
			pathToFileInfo.put(FileUtil.dir + "/" + startTest, new FileCoreInfo(originHash, file.length(), FileUtil.dir + "/" + startTest));
			ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
			Vector<String> filePaths = new Vector<String>();
			filePaths.add(FileUtil.dir + "/" + startTest);
			checksumToPath.put(originHash, filePaths);
			UserInfo userInfo = new UserInfo(InetAddress.getByName(ip), port, "test", 0L, 0L, pathToFileInfo, checksumToPath);
			FileConnectionThread fileThread = new FileConnectionThread(userInfo, originHash, file.length(),
					FileUtil.dir + "/" + startTest, 0L, endIndex);
			fileThread.start();
			
			testServer.join();
			fileThread.join();
			
			File destFile = new File(FileUtil.dir + "/" + startTest);
			String contents = new String(Files.readAllBytes(destFile.toPath()));
			
			assertEquals(FileUtil.fileContents2.substring(0, endIndex), contents);
			
		} catch (IOException | NoSuchAlgorithmException | InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}
	
	@Test
	public void testGetFileEnd() {
		String originHash;
		File file = new File(FileUtil.dir + "/" + FileUtil.sourceFile);
		FileUtil.createFile("", endTest);
		try {
			originHash = FileHasher.getFileHash(FileUtil.dir + "/" + FileUtil.sourceFile);
			FileConnectionTestServer testServer = new FileConnectionTestServer(InetAddress.getByName(ip), port,
					FileUtil.dir + "/" + FileUtil.sourceFile, startIndex, file.length());
			testServer.start();

			ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
			pathToFileInfo.put(FileUtil.dir + "/" + endTest, new FileCoreInfo(originHash, file.length(), FileUtil.dir + "/" + endTest));
			ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
			Vector<String> filePaths = new Vector<String>();
			filePaths.add(FileUtil.dir + "/" + endTest);
			checksumToPath.put(originHash, filePaths);
			UserInfo userInfo = new UserInfo(InetAddress.getByName(ip), port, "test", 0L, 0L, pathToFileInfo, checksumToPath);
			FileConnectionThread fileThread = new FileConnectionThread(userInfo, originHash, file.length(),
					FileUtil.dir + "/" + endTest, startIndex, file.length());
			fileThread.start();
			
			testServer.join();
			fileThread.join();			
			
			File destFile = new File(FileUtil.dir + "/" + endTest);
			String contents = new String(Files.readAllBytes(destFile.toPath()));
			
			assertEquals(FileUtil.fileContents2.substring(startIndex, (int)file.length()), contents);
			
		} catch (IOException | NoSuchAlgorithmException | InterruptedException e) {
			e.printStackTrace();
			fail();
		}
	}

}
