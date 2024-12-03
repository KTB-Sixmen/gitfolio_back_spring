package com.be.gitfolio.notification.service;


import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.notification.domain.Notification;
import com.be.gitfolio.notification.service.port.NotificationRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

import static com.be.gitfolio.common.event.KafkaEvent.*;
import static com.be.gitfolio.notification.dto.NotificationResponseDTO.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class NotificationService {

    private final NotificationRepository notificationRepository;


    /**
     * 알림 생성
     * @param resumeEvent
     */
    @Transactional
    @KafkaListener(topics = "resumeEventTopic", groupId = "notification-group")
    public Notification createNotification(ResumeEvent resumeEvent) {
        log.info("Kafka 메시지 도착 : {}", resumeEvent.getSenderNickname());
        if (!Objects.equals(resumeEvent.getSenderId(), resumeEvent.getReceiverId())) {
            Notification notification = Notification.from(resumeEvent);
            return notificationRepository.save(notification);
        }
        return null;
    }

    /**
     * 내 알림 전체 조회
     * @param receiverId
     * @return List<NotificationListDTO>
     */
    public List<NotificationListDTO> getMyNotificationList(Long receiverId) {
        List<Notification> notifications = notificationRepository.findAllByReceiverIdAndReadFalse(receiverId);
        return notifications.stream()
                .map(NotificationListDTO::from)
                .toList();
    }

    /**
     * 알림 단건 읽기
     * @param memberId
     * @param notificationId
     * @return NotificationListDTO
     */
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

    /**
     * 알림 생성 (webClient용)
     * @param resumeEvent
     */
    @Transactional
    public void create(ResumeEvent resumeEvent) {
        log.info("webClient요청 도착");
        Notification notification = Notification.from(resumeEvent);
        notificationRepository.save(notification);
    }
}
