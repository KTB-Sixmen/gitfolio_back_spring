package com.be.gitfolio.notification.service.port;

import com.be.gitfolio.notification.domain.Notification;

import java.util.List;
import java.util.Optional;

public interface NotificationRepository {
    Notification save(Notification notification);

    Optional<Notification> findById(Long notificationId);

    List<Notification> findAllByReceiverIdAndReadFalse(Long receiverId);

    void deleteAllByMemberId(Long memberId);
}
