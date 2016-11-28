package studyproject.Test.Core;

import static org.junit.Assert.*;

import org.junit.Test;

import studyproject.API.Core.File.InfoList.FileInfoListEntry;
import studyproject.API.Core.File.Watcher.FileWatcherTreeNode;

public class FileWatcherTreeNodeTest {
	
	private String testDirectory = "java-code/testData/FileWatcherController/";

	@Test
	public void shouldFindOneFile() throws Exception {
		String fileName = testDirectory+"oneFile/testFile.txt";
		FileInfoListEntry entry = new FileInfoListEntry(fileName);
		
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry.fileName), entry);
		
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName);
		assertNotNull(found);
		assertEquals(found.fileName, fileName);
	}
	
	@Test
	public void shouldFindNoFile() throws Exception {
		String fileName = testDirectory+"oneFile/testFile.txt";
		FileInfoListEntry entry = new FileInfoListEntry(fileName);
		
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry.fileName), entry);
		
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName+"1");
		assertNull(found);
	}
	
}
