package studyproject.API.Lvl.Mid.ThreadMonitoring;

import javafx.application.Platform;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.filesTree.FilesTreeView;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.logging.LogKey;

import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class manages the download of a "Folder" For each file a new
 * {@link FileConnectionThread} is started serially after one was finished.
 * 
 * @author ninti
 */
public class MultipleDownloadHelper extends Thread implements MonitoredThread {
	private Vector<FileCoreInfo> assignedDownloads;
	private UserInfo user;
	private SimpleDoubleProperty progress = new SimpleDoubleProperty(0);
	private long wholeSize = 0;
	private SimpleLongProperty doneSize = new SimpleLongProperty(0);
	private SimpleStringProperty currentDownloadedFile = new SimpleStringProperty("");
	private String pathToDownloadTo = "";
	private boolean oneOfMultiple = false;
	private String baseDir = "";
	private Logger logger = Logger.getGlobal();
	private SimpleBooleanProperty finished = new SimpleBooleanProperty(false);
	private SimpleBooleanProperty running = new SimpleBooleanProperty(true);
	private FileConnectionThread currentFileConnectionThread = null;

	/**
	 * Creates a new Instance of {@link MultipleDownloadHelper} which starts a
	 * {@link FileConnectionThread} for each {@link FileCoreInfo} given in the
	 * Vector
	 * 
	 * @param assignedDownloads
	 *            the vector list with {@link FileCoreInfo}s to download
	 * @param user
	 *            the user to get the files from
	 * @param pathToDownloadTo
	 *            the path to download the files to
	 * @param baseDir
	 *            the basedir aka the foldername which contains the files, aka
	 *            the parent of the {@link FileCoreInfo}s in the
	 *            {@link FilesTreeView}
	 */
	public MultipleDownloadHelper(Vector<FileCoreInfo> assignedDownloads, UserInfo user, String pathToDownloadTo,
			String baseDir) {
		this.assignedDownloads = assignedDownloads;
		this.user = user;
		this.baseDir = baseDir;
		this.pathToDownloadTo = pathToDownloadTo;
		determineWholeFileSize();
	}

	/**
	 * calculates the size of the {@link FileCoreInfo} vector list
	 */
	private void determineWholeFileSize() {
		for (FileCoreInfo info : assignedDownloads) {
			wholeSize += info.getFilesize();
		}
	}

	@Override
	public void run() {
		int i = 0;
		while (running.get() == true && i < assignedDownloads.size()) {
			FileCoreInfo currentFile = assignedDownloads.get(i);
			Platform.runLater(() -> currentDownloadedFile.set(currentFile.getFileName()));
			currentFileConnectionThread = new FileConnectionThread(user, currentFile.getChecksum(),
					currentFile.getFilesize(), pathToDownloadTo + currentFile.getFilePath());
			currentFileConnectionThread.setDaemon(true);
			currentFileConnectionThread.setOneOfMultiple(true);
			currentFileConnectionThread.getDoneSize().addListener((observable, oldValue, newValue) -> {
				Platform.runLater(() -> {
					doneSize.setValue(doneSize.get() + (newValue.longValue() - oldValue.longValue()));
					progress.set((double) doneSize.get() / (double) wholeSize);
				});
			});
			// TODO ninti: thread was setted to daemon, but it's not submitted
			// to the threadExeccutor from lodds, fix this when testing with
			// others
			currentFileConnectionThread.start();
			try {
				currentFileConnectionThread.join();
				i++;
			} catch (InterruptedException e) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, e));
			}
		}
		finished.setValue(true);
	}

	@Override
	public synchronized SimpleStringProperty getCurrentFileName() {
		return currentDownloadedFile;
	}

	@Override
	public synchronized SimpleLongProperty getDoneSize() {
		return doneSize;
	}

	@Override
	public synchronized long getWholeSize() {
		return wholeSize;
	}

	@Override
	public synchronized SimpleDoubleProperty getProgress() {
		return progress;
	}

	@Override
	public boolean isOneOfMultiple() {
		return oneOfMultiple;
	}

	@Override
	public void setOneOfMultiple(boolean oneOfMultiple) {
		this.oneOfMultiple = oneOfMultiple;
	}

	@Override
	public SimpleBooleanProperty isRunning() {
		return running;
	}

	@Override
	public void setRunning(boolean toSet) {
		if (currentFileConnectionThread != null)
			currentFileConnectionThread.setRunning(false);
		running.setValue(toSet);
	}

	@Override
	public synchronized SimpleBooleanProperty isFinished() {
		return finished;
	}

	@Override
	public String getNameToDisplay() {
		return baseDir;
	}
}
