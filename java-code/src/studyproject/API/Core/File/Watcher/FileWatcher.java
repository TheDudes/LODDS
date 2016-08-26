package studyproject.API.Core.File.Watcher;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_DELETE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;


import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;


public class FileWatcher {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		System.out.println("Start");

		FileWatcher myWatchService = new FileWatcher();

		myWatchService.watchDirectory("/Users/robinhood/Desktop");
	}

	public void watchDirectory(String path) {
		try {
				WatchService watcher = FileSystems.getDefault().newWatchService();

				// Register
				Path dir = Paths.get(path);
				dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);

				while (true) {
				    WatchKey key;
				    try {
				        // wait for a key to be available
				        key = watcher.take();
				    } catch (InterruptedException ex) {
				        return;
				    }

				    for (WatchEvent<?> event : key.pollEvents()) {
				        // get event type
				        WatchEvent.Kind<?> kind = event.kind();

				        // get file name
				        @SuppressWarnings("unchecked")
				        WatchEvent<Path> ev = (WatchEvent<Path>) event;
				        Path fileName = ev.context();
				        
				        System.out.println(kind.name() + ": " + fileName);

				        if (kind == OVERFLOW) {
				            
				        	// TODO: DEL
				        	
				        } else if (kind == ENTRY_CREATE) {
				        		
				            // TODO: ADD

				        } else if (kind == ENTRY_DELETE) {

				            // TODO: DEL

				        } else if (kind == ENTRY_MODIFY) {

				            // TODO: DEL & ADD

				        }
				    }

				    // Reset key
				    boolean valid = key.reset();
				    if (!valid) {
				        break;
				    }
			}
		}
		catch (IOException ex) {
	            System.err.println(ex);
	  }
	}

}
