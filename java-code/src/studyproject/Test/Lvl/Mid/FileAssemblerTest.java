package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Vector;

import org.junit.Test;

import studyproject.API.Loadbalancer.FileAssembler;
import studyproject.API.Loadbalancer.ProgressInfo;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.Test.FileUtil;

/**
 * Junit test for the FileAssembler class Only tests the functionality of
 * assembling files when the thread finished successfully The ability to start a
 * new thread for a failed thread is not tested (and would require lots of work)
 * 
 * @author Michael
 *
 */
public class FileAssemblerTest {

	private String finalFileName = "assembledFile";
	private String tmpFilePaths = "tmp/filePart";

	/**
	 * test if a file split into three parts is assembled correctly
	 */
	@Test
	public void testFileAssembly() {
		FileUtil.createFile("", finalFileName);
		int chunksTotal = 3;
		FileUtil.createFile(FileUtil.fileContents2.substring(0, 1000), tmpFilePaths + "0", false);
		FileUtil.createFile(FileUtil.fileContents2.substring(1000, 3000), tmpFilePaths + "1", false);
		FileUtil.createFile(FileUtil.fileContents2.substring(3000, FileUtil.fileContents2.length()), tmpFilePaths + "2",
				false);
		Vector<ProgressInfo> chunkThreads = new Vector<>();
		chunkThreads.add(new ProgressInfo(0, 1000, 0, tmpFilePaths, new FileConnectionThread(null, "", 1, "")));
		chunkThreads.add(new ProgressInfo(1000, 3000, 1, tmpFilePaths, new FileConnectionThread(null, "", 1, "")));
		chunkThreads.add(new ProgressInfo(3000, FileUtil.fileContents2.length(), 2, tmpFilePaths,
				new FileConnectionThread(null, "", 1, "")));
		chunkThreads.get(0).setFinishedSuccessfully(true);
		chunkThreads.get(1).setFinishedSuccessfully(true);
		chunkThreads.get(2).setFinishedSuccessfully(true);

		FileAssembler fileAssembler = new FileAssembler(FileUtil.dir + "/" + tmpFilePaths,
				FileUtil.dir + "/" + finalFileName, chunksTotal, chunkThreads);
		fileAssembler.setChunkReady(0, 1000 - 0);
		fileAssembler.setChunkReady(1, 3000 - 1000);
		fileAssembler.setChunkReady(2, FileUtil.fileContents2.length() - 3000);
		fileAssembler.start();

		try {
			fileAssembler.join();

			File destFile = new File(FileUtil.dir + "/" + finalFileName);
			String contents = new String(Files.readAllBytes(destFile.toPath()));

			assertEquals(FileUtil.fileContents2, contents);
		} catch (InterruptedException | IOException e) {
			e.printStackTrace();
			fail();
			FileUtil.cleanUp();
		}
		FileUtil.cleanUp();
	}

}
