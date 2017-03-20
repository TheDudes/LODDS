package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.API.Core.File.Watcher.FileWatcherController;
import studyproject.API.Errors.ErrorFactory;
import studyproject.logging.LogKey;

public class ShareFolderThread extends Thread {

	private String path;
	private FileWatcherController watchService;
	private Vector<String> sharedFolders;
	
	public ShareFolderThread(String path, FileWatcherController watchService) {
		this.path = watchService.addSlashToFileNameIfNecessary(path);
		this.watchService = watchService;
		this.sharedFolders = watchService.getWatchedDirectories();
	}
	
	@Override
	public void run() {
		
		if (Files.exists(Paths.get(path)) && Files.isDirectory(Paths.get(path))
				&& !sharedFolders.contains(path)) {

			try {
				watchService.watchDirectoryRecursively(path);
			} catch (NoSuchAlgorithmException e) {
				Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "NoSuchAlgorithmException thrown: ", e));
			} catch (IOException e) {
				Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: fileNotFound: " + e));
			} catch (Exception e) {
				Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "watchDirectoryRecursively-Exception thrown: " + e));
			}
		}
	}
}
