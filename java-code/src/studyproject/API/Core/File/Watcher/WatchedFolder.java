package studyproject.API.Core.File.Watcher;

import java.nio.file.WatchKey;

public class WatchedFolder {

	String fullPath;
	String virtualRoot;
	WatchKey key;
	Boolean watchForNewFiles;
	
	public WatchedFolder(String fullPath, String virtualRoot, WatchKey key, Boolean watchForNewFiles) {
		super();
		this.fullPath = fullPath;
		this.virtualRoot = virtualRoot;
		this.key = key;
		this.watchForNewFiles = watchForNewFiles;
	}
	
}
