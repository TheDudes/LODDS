package studyproject.API.Lvl.Mid.ThreadMonitoring;

import studyproject.API.Loadbalancer.LoadbalancerMainThread;
import studyproject.API.Lvl.Mid.BroadcastListenerThread;
import studyproject.API.Lvl.Mid.BroadcastSenderThread;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.API.Lvl.Mid.FileSenderThread;
import studyproject.API.Lvl.Mid.GetFileWPThread;
import studyproject.API.Lvl.Mid.InfoSenderThread;
import studyproject.API.Lvl.Mid.RequestHandlerThread;
import studyproject.API.Lvl.Mid.SendFileWPThread;
import studyproject.API.Lvl.Mid.UpdateFileInfoThread;

/**
 * The thread type is determined by the instance of the runnable overgiven to
 * the getType function.
 * 
 * @author ninti
 *
 */
public enum ThreadType {
	none, fixed, getFile, sendFile, info;

	/**
	 * Determines the thread type of the overgiven runnable.
	 * 
	 * @param runnable
	 *            to determine its threadtype
	 * @return the ThreadType of the overgiven runnable
	 */
	public static ThreadType getType(Runnable runnable) {
		if (runnable instanceof BroadcastListenerThread || runnable instanceof BroadcastSenderThread
				|| runnable instanceof RequestHandlerThread)
			return ThreadType.fixed;

		if (runnable instanceof FileConnectionThread || runnable instanceof GetFileWPThread
				|| runnable instanceof LoadbalancerMainThread)
			return ThreadType.getFile;

		if (runnable instanceof FileSenderThread || runnable instanceof SendFileWPThread)
			return ThreadType.sendFile;

		if (runnable instanceof UpdateFileInfoThread || runnable instanceof InfoSenderThread)
			return ThreadType.info;
		return ThreadType.none;
	}

}
