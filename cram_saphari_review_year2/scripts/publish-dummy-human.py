#!/usr/bin/env python
import rospy
from saphari_msgs.msg import Human, BodyPart
from geometry_msgs.msg import Point32

def makeBodyPart(id, label, centroid, radius):
    b = BodyPart()
    b.id = id
    b.label = label
    b.centroid = centroid
    b.radius = radius

    return b

def makeHuman():
    h = Human()
    h.header.stamp = rospy.Time.now()
    h.header.frame_id = "base_link"
    h.bodyParts.append(makeBodyPart(0, 15, Point32(0.1, 0.1, 0.1), 0.1))
    h.bodyParts.append(makeBodyPart(1, 16, Point32(0.6, -0.2, 1.0), 0.15))

    return h

def talker():
    pub = rospy.Publisher('saphari/human', Human)
    rospy.init_node('dummy_human_publisher')
    while not rospy.is_shutdown():
        pub.publish(makeHuman())
        rospy.sleep(0.2)

if __name__ == '__main__':
    try:
        talker()
    except rospy.ROSInterruptException:
        pass
