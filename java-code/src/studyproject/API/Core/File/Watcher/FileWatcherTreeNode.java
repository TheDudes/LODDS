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
	private Boolean isRoot;
	
	public FileWatcherTreeNode(boolean isRoot) {
		children = new ConcurrentHashMap<String, FileWatcherTreeNode>();
		this.isRoot = isRoot;
		
		if (isRoot) {
			fileName = "root";
		}
	}
	
	public static void main(String[] args) throws Exception {
		String fileName = "/Users/robinhood/Desktop/testData/test.zip";
		FileInfoListEntry entry = new FileInfoListEntry(fileName);
		
		String fileName2 = "/Users/robinhood/Desktop/testData/ahoi/ahoi.zip";
		FileInfoListEntry entry2 = new FileInfoListEntry(fileName2);

		
		FileWatcherTreeNode root = new FileWatcherTreeNode(true);
		root.addFileInfoEntry(convertFileNameToStringList(entry.fileName), entry);
		root.addFileInfoEntry(convertFileNameToStringList(entry2.fileName), entry2);

		System.out.println("Tree:");
		root.printTree(0);
		
		System.out.println("\n\nTest FileInfoListEntry:");
		FileInfoListEntry found = root.getFileInfoListEntryByFileName(fileName);
	}
	
	public void printTree(Integer depth) {
		System.out.println("");
		
		// Make depth lines e.g. ---- file
		for (int k=0; k<depth; k++) {
			System.out.print("-");
		}
		
		System.out.print(fileName);
		
		for (String fileName:children.keySet()) {
			children.get(fileName).printTree(depth+1);
		}
	}
	
	public FileWatcherTreeNode getNodeByFileName(String fileName) {
		return getNodeBySubDirs(convertFileNameToStringList(fileName));
	}
	
	public FileInfoListEntry getFileInfoListEntryByFileName(String fileName) {
		System.out.println("Searching for file: "+fileName);
		FileWatcherTreeNode foundNode = getNodeByFileName(fileName);
		
		if (foundNode == null) {
			System.out.println("File not found: "+fileName);
			return null;
		} else {
			System.out.println("File found: "+foundNode.fileName);
			return foundNode.fileInfo;		
		}

	}
	
	public static List<String> convertFileNameToStringList(String fileName) {
		String[] subDirs = fileName.split(Pattern.quote(File.separator));
		List<String> list = new LinkedList<String>(Arrays.asList(subDirs));
		list.remove(0);
		return list;
	}
	
	public FileWatcherTreeNode getNodeBySubDirs(List<String> subDirsList) {
		
		System.out.println("SubDirList: "+subDirsList);
		System.out.println("Current node: "+this.fileName);

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

		} else {
			System.out.println("Node '"+subDirsList.get(0)+"' not found in "+this.fileName);
		}
		
		return null;
	}

	public void addFileInfoEntry(List<String> subDirsList, FileInfoListEntry fileInfo) throws Exception {

		// If child already exists, go on
		if (children.containsKey(subDirsList.get(0))) {
			FileWatcherTreeNode child = children.get(subDirsList.get(0));
			
			if (subDirsList.size() == 1) {
				throw new Exception("child for that file name already exists");
			}
			
			// Remove first folder
			subDirsList.remove(0);
			
			// Add remaining list to child
			child.addFileInfoEntry(subDirsList, fileInfo);
			
		// Child does not exist -> Add
		} else {

				// We need to create new child
				FileWatcherTreeNode newNode = new FileWatcherTreeNode(false);
				newNode.fileName = subDirsList.get(0);
				newNode.parent = this;
				
				// Add new node as child to current node
				children.put(newNode.fileName, newNode);
				
				if (subDirsList.size() != 1) {
					
					// Remove current entry from list
					subDirsList.remove(0);
					
					// Start new add process from newNode
					newNode.addFileInfoEntry(subDirsList, fileInfo);	

				}	
		}
	}
}