package studyproject.API.Lvl.Mid.ThreadMonitoring;

import java.util.Vector;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import studyproject.App;
import studyproject.API.Lvl.Mid.ShareFolderThread;
import studyproject.API.Lvl.Mid.Lodds.LoddsModel;

/**
 * This is an executor who splits incoming runnables into 5
 * {@link ExecutorService}s, according to their {@link ThreadType}.
 * 
 * 
 * @author ninti
 *
 */
public class ThreadExecutor implements Executor {
	private final int DEFAULT_AT_A_TIME_UPLOADS = Integer
			.valueOf(App.properties.getProperty("DEFAULT_AT_A_TIME_UPLOADS"));
	private final int DEFAULT_AT_A_TIME_DOWNLOADS = Integer
			.valueOf(App.properties.getProperty("DEFAULT_AT_A_TIME_DOWNLOADS"));
	private final int DEFAULT_AT_A_TIME_SHARES = Integer
			.valueOf(App.properties.getProperty("DEFAULT_AT_A_TIME_SHARES"));

	private final int NR_OF_FIXED_THREADS = 5;

	private ExecutorService infoExecutor;
	private ExecutorService sendFileExecutor;
	private ExecutorService getFileExecutor;
	private ExecutorService fixedThreadExecutor;
	private ExecutorService shareFolderExecutor;
	private ThreadFactoryBuilder threadFactoryBuilder;
	private Vector<ExecutorService> allExecutors = new Vector<ExecutorService>();
	private LoddsModel loddsModel;

	/**
	 * Default Constructor, initializes the threadFactoryBuilder and all
	 * Executors<br>
	 * <br>
	 * - infoExecutor - For all info threads <br>
	 * - sendFileExecutor - For Upload<br>
	 * - getFileExecutor - For Downloads <br>
	 * - fixedThreadExecutor - For all fixed Threads <br>
	 * - shareFolderExecutor - {@link ShareFolderThread}<br>
	 * <br>
	 * For more information about which Thread is executed by which executor
	 * refer to {@link ThreadType} and this classes execute method
	 */
	public ThreadExecutor(LoddsModel loddsModel) {
		threadFactoryBuilder = new ThreadFactoryBuilder();
		threadFactoryBuilder.setDaemon(true);
		threadFactoryBuilder.setNamePrefix("fixedThreads");
		fixedThreadExecutor = Executors.newFixedThreadPool(NR_OF_FIXED_THREADS, threadFactoryBuilder.build());
		threadFactoryBuilder.setNamePrefix("info");
		infoExecutor = Executors.newCachedThreadPool(threadFactoryBuilder.build());
		threadFactoryBuilder.setNamePrefix("fileSender");
		sendFileExecutor = Executors.newFixedThreadPool(DEFAULT_AT_A_TIME_UPLOADS, threadFactoryBuilder.build());
		threadFactoryBuilder.setNamePrefix("fileGetter");
		getFileExecutor = Executors.newFixedThreadPool(DEFAULT_AT_A_TIME_DOWNLOADS, threadFactoryBuilder.build());
		threadFactoryBuilder.setNamePrefix("shareFolder");
		shareFolderExecutor = Executors.newFixedThreadPool(DEFAULT_AT_A_TIME_SHARES, threadFactoryBuilder.build());
		addExecutorsToVector();
		this.loddsModel = loddsModel;
	}

	/**
	 * adds all {@link ExecutorService}s of this class to the allExecutor vector
	 * of this class
	 */
	private void addExecutorsToVector() {
		allExecutors.addElement(sendFileExecutor);
		allExecutors.addElement(infoExecutor);
		allExecutors.addElement(getFileExecutor);
		allExecutors.addElement(fixedThreadExecutor);
		allExecutors.addElement(shareFolderExecutor);
	}

	@Override
	public void execute(Runnable runnable) {
		ThreadType threadType = ThreadType.getType(runnable);
		switch (threadType) {
		case none:
			return;
		case fixed:
			fixedThreadExecutor.submit(runnable);
			break;
		case info:
			infoExecutor.submit(runnable);
			break;
		case getFile:
			getFileExecutor.submit(runnable);
			break;
		case sendFile:
			sendFileExecutor.submit(runnable);
			break;
		case shareFolder:
			shareFolderExecutor.submit(runnable);
		default:
			break;
		}

		if (runnable instanceof MonitoredThread) {
			addToList((MonitoredThread) runnable);
		}
	}

	/**
	 * Adds a {@link MonitoredThread} to the {@link LoddsModel}s tasklist
	 * 
	 * @param monitoredThread
	 *            the monitored thread to add to the task list
	 */
	private void addToList(MonitoredThread monitoredThread) {
		loddsModel.getTasksList().add(monitoredThread);
	}

	/**
	 * Calls shutdownNow on all {@link ExecutorService}s in the allExecutor
	 * vector of this class
	 */
	public void stopAllExcutors() {
		allExecutors.forEach(ExecutorService::shutdownNow);
	}
}
