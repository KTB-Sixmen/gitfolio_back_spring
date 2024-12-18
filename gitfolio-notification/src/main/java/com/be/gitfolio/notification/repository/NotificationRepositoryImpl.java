package com.be.gitfolio.notification.repository;

import com.be.gitfolio.notification.domain.Notification;
import com.be.gitfolio.notification.service.port.NotificationRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class NotificationRepositoryImpl implements NotificationRepository {

    private final NotificationJpaRepository notificationJpaRepository;

    @Override
    public Notification save(Notification notification) {
        return notificationJpaRepository.save(notification);
    }

    @Override
    public Optional<Notification> findById(Long notificationId) {
        return notificationJpaRepository.findById(notificationId);
    }

    @Override
    public List<Notification> findAllByReceiverIdAndReadFalse(Long receiverId) {
        return notificationJpaRepository.findAllByReceiverIdAndReadFalse(receiverId);
    }

    @Override
    public void deleteAllByMemberId(Long memberId) {
        notificationJpaRepository.deleteAllByMemberId(memberId);
    }
}
