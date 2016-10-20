package studyproject.API.Core.File.Watcher;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import studyproject.API.Core.File.InfoList.FileInfoListEntry;

public class FileWatcherTreeNode {

	private String fileName;
	private ConcurrentHashMap<String, FileWatcherTreeNode> children;
	private FileInfoListEntry fileInfo;
	private FileWatcherTreeNode parent;
	
	public FileWatcherTreeNode() {
		children = new ConcurrentHashMap<String, FileWatcherTreeNode>();
	}
	
	public static void main(String[] args) throws Exception {
		String fileName = "/Users/robinhood/Desktop/testData/test.zip";
		FileInfoListEntry entry = new FileInfoListEntry(fileName);
		
		FileWatcherTreeNode root = new FileWatcherTreeNode();
		root.addFileInfoEntry(convertFileNameToStringList(entry.fileName), entry);
		
		System.out.println("Tree:");
		root.printTree(0);
		
		System.out.println("\n\nTest FileInfoListEntry:");
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName);
		System.out.println(found.checksum);
	}
	
	public void printTree(Integer depth) {
		Iterator<String> i = children.keySet().iterator();
		if (i.hasNext()) {
			String fileName = i.next();
			
			System.out.println("");
			
			for (int k=0; k<depth; k++) {
				System.out.print("-");
			}
			
			System.out.print(fileName);
			
			children.get(fileName).printTree(depth+1);
		}
	}
	
	public FileWatcherTreeNode getNodeByFileName(String fileName) {
		return getNodeBySubDirs(convertFileNameToStringList(fileName));
	}
	
	public FileInfoListEntry getFileInfoListEntryByFileName(String fileName) {
		return getNodeBySubDirs(convertFileNameToStringList(fileName)).fileInfo;
	}
	
	public static List<String> convertFileNameToStringList(String fileName) {
		String[] subDirs = fileName.split(Pattern.quote(File.separator));
		List<String> list = new LinkedList<String>(Arrays.asList(subDirs));
		list.remove(0);
		return list;
	}
	
	public FileWatcherTreeNode getNodeBySubDirs(List<String> subDirsList) {

		// Check if child contains first folder
		if (children.containsKey(subDirsList.get(0))) {
			System.out.println("getNodeBySubDirs. File found: "+subDirsList.get(0));
			
			FileWatcherTreeNode child = children.get(subDirsList.get(0));
			
			if (subDirsList.size() == 1)
				return child;
			
			// Remove first folder
			subDirsList.remove(0);
			
			// Search again
			return child.getNodeBySubDirs(subDirsList);

		}
		
		return null;
	}

	public void addFileInfoEntry(List<String> subDirsList, FileInfoListEntry fileInfo) throws Exception {
		// System.out.println("Print list: ");
		// System.out.println(subDirsList);

		// If child already exists, go on
		if (children.containsKey(subDirsList.get(0))) {
			FileWatcherTreeNode child = children.get(subDirsList.get(0));
			
			if (subDirsList.size() == 1) {
				throw new Exception("child for that file name already exists");
			}
			
			// Remove first folder
			subDirsList.remove(0);
			
			// Search again
			child.addFileInfoEntry(subDirsList, fileInfo);
			
		// Child does not exist -> Add
		} else {

			// If it is the last folder, add fileInfo to current child
			if (subDirsList.size() == 1) {
				this.fileInfo = fileInfo;
			} else {
				// We need to create new child
				FileWatcherTreeNode newNode = new FileWatcherTreeNode();
				newNode.fileName = subDirsList.get(0);
				newNode.parent = this;
				subDirsList.remove(0);
				children.put(subDirsList.get(0), newNode);
				newNode.addFileInfoEntry(subDirsList, fileInfo);
			}
			
		}
	}
}