package studyproject.API.Lvl.Mid.ThreadMonitoring;

import java.util.Vector;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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
	//TODO expose list or hash map with threads
	/**
	 * Default Constructor, initialises the threadFactoryBuilder and 
	 */
	public ThreadExecutor() {
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
		if (threadType == ThreadType.fixed)
			fixedThreadExecutor.submit(runnable);
		if (threadType == ThreadType.info)
			infoExecutor.submit(runnable);
		if (threadType == ThreadType.getFile)
			getFileExecutor.submit(runnable);
		if (threadType == ThreadType.sendFile)
			sendFileExecutor.submit(runnable);
	}
}
