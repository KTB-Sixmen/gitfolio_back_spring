package com.be.gitfolio.notification.service;


import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.notification.domain.Notification;
import com.be.gitfolio.notification.dto.NotificationResponseDTO;
import com.be.gitfolio.notification.repository.NotificationRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static com.be.gitfolio.common.event.KafkaEvent.*;
import static com.be.gitfolio.notification.dto.NotificationResponseDTO.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class NotificationService {

    private final NotificationRepository notificationRepository;


    /**
     * 알림 생성 TODO : 내가 나에게 알림 보내지 않도록 수정
     */
    @Transactional
    @KafkaListener(topics = "resumeEventTopic", groupId = "notification-group")
    public void consumeResumeEvent(ResumeEvent resumeEvent) {
        log.info("Kafka 메시지 도착 : {}", resumeEvent.getType());
        Notification notification = Notification.from(resumeEvent);
        notificationRepository.save(notification);
    }

    /**
     * 내 알림 전체 조회
     */
    public List<NotificationListDTO> getMyNotificationList(Long receiverId) {
        List<Notification> notifications = notificationRepository.findAllByReceiverIdAndReadFalse(receiverId);
        return notifications.stream()
                .map(NotificationListDTO::from)
                .toList();
    }

    @Transactional
    public NotificationListDTO getNotification(Long memberId, Long notificationId) {
        Notification notification = notificationRepository.findById(notificationId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_NOTIFICATION_INFO));

        if (!notification.getReceiverId().equals(memberId)) {
            throw new BaseException(ErrorCode.INVALID_MEMBER_TO_READ_NOTIFICATION);
        }
        notification.readNotification();
        notificationRepository.save(notification);

        return NotificationListDTO.from(notification);
    }
}
