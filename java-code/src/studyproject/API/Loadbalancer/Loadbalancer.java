package studyproject.API.Loadbalancer;

import studyproject.API.Lvl.Mid.LODDS;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * 
 * @author Michael
 *
 */
public class Loadbalancer {

	private LODDS loddsobject;
	private long loadBalancingMinumum;
	private int parallelDownloads;

	public Loadbalancer(LODDS loddsobject, long loadBalancingMinumum, int parallelDownloads) {
		this.loddsobject = loddsobject;
		this.loadBalancingMinumum = loadBalancingMinumum;
		this.parallelDownloads = parallelDownloads;
	}

	public synchronized void splitLoad(String checksum, String localPath,
			long fileSize) {
		if (loddsobject.getFileSize(checksum) < loadBalancingMinumum) {
			loddsobject
					.getFile(getClientMinLoad(checksum), checksum, localPath);
		} else {
			// TODO start thread that assigns how much is taken from which user,
			// checks the threads to get the parts of the file and, after all
			// threads finished, puts the temporary files together
			LoadbalancerMainThread loadbalancerMainThread = new LoadbalancerMainThread(
					loddsobject, checksum, localPath,
					fileSize, loadBalancingMinumum, parallelDownloads);
			loadbalancerMainThread.start();
		}
	}

	/**
	 * 
	 * @return the client that has the specified and has the lowest amount of
	 *         load or the first client in the list that has no load
	 */
	private synchronized String getClientMinLoad(String checksum) {
		long minLoad = Long.MAX_VALUE;
		String clientWithLowestLoad = "";
		for (UserInfo userInfo : loddsobject.getOwningUsers(checksum)) {
			if (userInfo.getLoad() == 0) {
				return userInfo.getUserName();
			} else if (userInfo.getLoad() < minLoad) {
				clientWithLowestLoad = userInfo.getUserName();
				minLoad = userInfo.getLoad();
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

	public int getParallelDownloads() {
		return parallelDownloads;
	}

	public void setParallelDownloads(int parallelDownloads) {
		this.parallelDownloads = parallelDownloads;
	}

}
