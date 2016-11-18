package studyproject.API.Lvl.Mid.ThreadMonitoring;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Thread Factory class to set a useful name prefix, the daemon boolean or the
 * thread priority
 * 
 * @author ninti
 *
 */
public class ThreadFactoryBuilder {

	private String namePrefix = null;
	private boolean daemon = false;
	private int priority = Thread.NORM_PRIORITY;

	
	public ThreadFactoryBuilder setNamePrefix(String namePrefix) {
		if (namePrefix == null) {
			throw new NullPointerException();
		}
		this.namePrefix = namePrefix;
		return this;
	}

	public ThreadFactoryBuilder setDaemon(boolean daemon) {
		this.daemon = daemon;
		return this;
	}

	public ThreadFactoryBuilder setPriority(int priority) {
		if (priority < Thread.MIN_PRIORITY)
			return this;

		if (priority > Thread.MAX_PRIORITY) {
			return this;
		}

		this.priority = priority;
		return this;
	}

	public ThreadFactory build() {
		return build(this);
	}

	private static ThreadFactory build(ThreadFactoryBuilder builder) {
		final String namePrefix = builder.namePrefix;
		final Boolean daemon = builder.daemon;
		final Integer priority = builder.priority;

		final AtomicLong count = new AtomicLong(0);

		return new ThreadFactory() {
			@Override
			public Thread newThread(Runnable runnable) {
				Thread thread = new Thread(runnable);
				if (namePrefix != null) {
					thread.setName(namePrefix + "-" + count.getAndIncrement());
				}
				if (daemon != null) {
					thread.setDaemon(daemon);
				}
				if (priority != null) {
					thread.setPriority(priority);
				}
				return thread;
			}
		};
	}

}
