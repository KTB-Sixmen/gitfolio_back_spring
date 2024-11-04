package com.be.gitfolio.notification.repository;

import com.be.gitfolio.notification.domain.Notification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NotificationRepository extends JpaRepository<Notification, Long> {
    List<Notification> findAllByReceiverIdAndReadFalse(Long receiverId);


}
