package studyproject.API.Loadbalancer;

import java.util.Vector;
import java.util.concurrent.Executor;

import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * 
 * @author Michael
 *
 */
public class Loadbalancer {

	private long loadBalancingMinumum;
	private int parallelDownloads;
	private Executor executor;

	/**
	 * 
	 * @param loadBalancingMinumum
	 *            the size at which this client uses loadbalancing
	 * @param parallelDownloads
	 *            the maximum number of concurrent downloads used to pull this
	 *            file
	 * @param executor
	 *            the thread executor to execute all threads
	 */
	public Loadbalancer(long loadBalancingMinumum, int parallelDownloads, Executor executor) {
		this.loadBalancingMinumum = loadBalancingMinumum;
		this.parallelDownloads = parallelDownloads;
		this.executor = executor;
	}

	/**
	 * 
	 * @param owningUsers
	 *            the vector with all users owning the specified file
	 * @param checksum
	 *            the checksum of the specified file
	 * @param localPath
	 *            the complete path to the location where the file should be
	 *            placed
	 * @param fileSize
	 *            the total size of the file
	 */
	public synchronized void splitLoad(Vector<UserInfo> owningUsers, String checksum, String localPath, long fileSize) {
		LoadbalancerMainThread loadbalancerMainThread = new LoadbalancerMainThread(owningUsers, checksum, localPath,
				fileSize, loadBalancingMinumum, parallelDownloads, executor);
		executor.execute(loadbalancerMainThread);
	}

	/**
	 * 
	 * @return the client that has the specified file and has the lowest amount
	 *         of load or the first client in the list that has no load
	 */
	public synchronized String getClientMinLoad(Vector<UserInfo> owningUsers, String checksum) {
		long minLoad = Long.MAX_VALUE;
		String clientWithLowestLoad = "";
		for (UserInfo userInfo : owningUsers) {
			if(userInfo.getChecksumToPath().containsKey(checksum)){
				if (userInfo.getLoad() == 0) {
					return userInfo.getUserName();
				} else if (userInfo.getLoad() < minLoad) {
					clientWithLowestLoad = userInfo.getUserName();
					minLoad = userInfo.getLoad();
				}
			}
		}
		return clientWithLowestLoad;
	}

	/**
	 * 
	 * @return the minimum size before a file is split for load balancing if
	 *         there is more than one client sharing that file
	 */
	public synchronized long getLoadBalancingMinumum() {
		return loadBalancingMinumum;
	}

	/**
	 * 
	 * @param loadBalancingMinumum
	 *            the minimum size before a file is split for load balancing if
	 *            there is more than one client sharing that file
	 */
	public synchronized void setLoadBalancingMinumum(long loadBalancingMinumum) {
		this.loadBalancingMinumum = loadBalancingMinumum;
	}

	/**
	 * 
	 * @return the maximum number of concurrent downloads used to pull this file
	 */
	public int getParallelDownloads() {
		return parallelDownloads;
	}

	/**
	 * 
	 * @param parallelDownloads
	 *            the maximum number of concurrent downloads used to pull this
	 *            file
	 */
	public void setParallelDownloads(int parallelDownloads) {
		this.parallelDownloads = parallelDownloads;
	}

}
