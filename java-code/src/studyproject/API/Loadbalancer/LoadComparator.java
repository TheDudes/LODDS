package studyproject.API.Loadbalancer;

import java.util.Comparator;

import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LoadComparator implements Comparator<UserInfo> {

	@Override
	public int compare(UserInfo user1, UserInfo user2) {
		if (user1.getLoad() > user2.getLoad()) {
			return 1;
		} else if (user1.getLoad() < user2.getLoad()) {
			return -1;
		} else {
			return 0;
		}
	}

}
