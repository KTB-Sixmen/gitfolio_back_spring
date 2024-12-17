package com.be.gitfolio.notification.repository;

import com.be.gitfolio.notification.domain.Notification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface NotificationJpaRepository extends JpaRepository<Notification, Long> {
    List<Notification> findAllByReceiverIdAndReadFalse(Long receiverId);

    @Modifying
    @Query("DELETE FROM Notification n WHERE n.senderId = :memberId OR n.receiverId = :memberId")
    void deleteAllByMemberId(@Param("memberId") Long memberId);
}
