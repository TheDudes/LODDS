package studyproject.API.Core.File.Watcher;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import studyproject.API.Core.File.InfoList.FileInfoListEntry;

public class FileWatcherTreeNode {

	private String fileName;
	private ConcurrentHashMap<String, FileWatcherTreeNode> children;
	private FileInfoListEntry fileInfo;
	private FileWatcherTreeNode parent;
	
	public FileWatcherTreeNode getNodeByFileName(String fileName) {
		// Split path
		String[] subDirs = fileName.split(Pattern.quote(File.separator));
		ArrayList<String> subDirsList = (ArrayList<String>) Arrays.asList(subDirs);
		return getNodeBySubDirs(subDirsList);
	}
	
	public FileWatcherTreeNode getNodeBySubDirs(ArrayList<String> subDirsList) {


		// Check if child contains first folder
		if (children.containsKey(subDirsList.get(0))) {
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

	public void addFileInfoEntry(ArrayList<String> subDirsList, FileInfoListEntry fileInfo) throws Exception {

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
			
			// We need to create new child
			FileWatcherTreeNode newNode = new FileWatcherTreeNode();
			newNode.fileName = subDirsList.get(0);
			newNode.parent = this;
			
			// If it is the last folder, add fileInfo
			if (subDirsList.size() == 1) {
				newNode.fileInfo = fileInfo;
				children.put(subDirsList.get(0), newNode);
			} else {
				subDirsList.remove(0);
				children.put(subDirsList.get(0), newNode);
				newNode.addFileInfoEntry(subDirsList, fileInfo);
			}
			
		}
	}
}