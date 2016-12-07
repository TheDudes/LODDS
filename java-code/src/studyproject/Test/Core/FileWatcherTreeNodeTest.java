package studyproject.Test.Core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;

import org.junit.Test;

import studyproject.API.Core.File.InfoList.FileInfoListEntry;
import studyproject.API.Core.File.Watcher.FileWatcherTreeNode;

public class FileWatcherTreeNodeTest {
	
	private String testDirectory = "java-code/testData/FileWatcherController/";

	@Test
	public void shouldFindOneFile() throws Exception {
		String fileName = testDirectory+"oneFile/testFile.txt";
		FileInfoListEntry entry = new FileInfoListEntry(fileName, testDirectory);
		
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry.fileName), entry);
		
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName);
		assertNotNull(found);
		assertEquals(found.fileName, fileName);
	}
	
	@Test
	public void shouldFindNoFile() throws Exception {
		String fileName = testDirectory+"oneFile/testFile.txt";
		FileInfoListEntry entry = new FileInfoListEntry(fileName, testDirectory);
		
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry.fileName), entry);
		
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName+"1");
		assertNull(found);
	}
	
	@Test
	public void shouldFindNoFileThatWasRemoved() throws Exception {
		
		// Create root
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		
		// Add first file
		String fileName = testDirectory+"oneFile/testFile.txt";
		FileInfoListEntry entry = new FileInfoListEntry(fileName, testDirectory);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry.fileName), entry);
		
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName);
		assertNotNull(found);
		assertEquals(found.fileName, fileName);
		
		// Add second file
		String fileName2 = "/Users/robinhood/Desktop/testData/ahoi/ahoi.zip";
		FileInfoListEntry entry2 = new FileInfoListEntry(fileName2, testDirectory);
		root.addFileInfoEntry(FileWatcherTreeNode.convertFileNameToStringList(entry2.fileName), entry2);
		
		FileInfoListEntry foundSecondFile = root.getFileInfoListEntryByFileName(fileName2);
		assertNotNull(foundSecondFile);
		assertEquals(foundSecondFile.fileName, fileName2);
		
		// Remove second file
		ArrayList<FileInfoListEntry> entries = FileWatcherTreeNode.removeFileNameAndGetRemovedFileInfoListEntries(root, fileName2);
		
		// One FileInfoListEntry should have been removed
		assertEquals(1, entries.size());
		
		// File should not be found anymore
		assertNull(root.getFileInfoListEntryByFileName(fileName2));

	}
	
}
