package studyproject.API.LowLvl;

import java.net.DatagramSocket;

import studyproject.API.Core.AdInfo;

public interface Advertises {

	public int sendAdvertise(DatagramSocket broadcastStream, AdInfo adInfo);

	public int readAdvertise(DatagramSocket broadcastStream, AdInfo adInfo);
}
