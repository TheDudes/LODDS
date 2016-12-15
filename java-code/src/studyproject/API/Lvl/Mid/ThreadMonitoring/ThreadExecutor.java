package studyproject.API.Lvl.Mid.ThreadMonitoring;

import java.util.Vector;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import studyproject.API.Lvl.Mid.Lodds.LoddsModel;

/**
 * This is an executor who splits incoming runnables into 4 Executors, according
 * to their {@link ThreadType}.
 * 
 * 
 * @author ninti
 *
 */
public class ThreadExecutor implements Executor {
	private final int DEFAULT_AT_A_TIME_UPLOADS = 20;
	private final int DEFAULT_AT_A_TIME_DOWNLOADS = 20;
	private final int NR_OF_FIXED_THREADS = 5;
	private ExecutorService infoExecutor;
	private ExecutorService sendFileExecutor;
	private ExecutorService getFileExecutor;
	private ExecutorService fixedThreadExecutor;
	private ThreadFactoryBuilder threadFactoryBuilder;
	private Vector<ExecutorService> allExecutors = new Vector<ExecutorService>();
	private LoddsModel loddsModel;

	/**
	 * Default Constructor, initialises the threadFactoryBuilder and
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
		addExecutorsToVector();
		this.loddsModel = loddsModel;
	}

	private void addExecutorsToVector() {
		allExecutors.addElement(sendFileExecutor);
		allExecutors.addElement(infoExecutor);
		allExecutors.addElement(getFileExecutor);
		allExecutors.addElement(fixedThreadExecutor);
	}

	@Override
	public void execute(Runnable runnable) {
		ThreadType threadType = ThreadType.getType(runnable);
		// TODO thread not executable error
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
		default:
			break;
		}

		if (runnable instanceof MonitoredThread) {
			addToList((MonitoredThread) runnable);
		}
	}

	private void addToList(MonitoredThread monitoredThread) {
		loddsModel.getTasksList().add(monitoredThread);
		monitoredThread.setSubmitted(true);
	}
}
