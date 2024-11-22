package com.be.gitfolio.notification.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.notification.dto.NotificationResponseDTO;
import com.be.gitfolio.notification.service.NotificationService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.be.gitfolio.notification.dto.NotificationResponseDTO.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/notifications")
public class NotificationController {

    private final NotificationService notificationService;

    /**
     * 내 알림 전체 조회
     */
    @AuthRequired
    @GetMapping("/me")
    public ResponseEntity<BaseResponse<List<NotificationListDTO>>> getMyNotificationList(HttpServletRequest request) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.ok().body(new BaseResponse<>(notificationService.getMyNotificationList(Long.valueOf(memberId))));
    }

    /**
     * 알림 조회
     */
    @AuthRequired
    @GetMapping("/{notificationId}")
    public ResponseEntity<BaseResponse<NotificationListDTO>> getNotification(HttpServletRequest request,
                                                                 @PathVariable("notificationId") Long notificationId) {
        String memberId = request.getAttribute("memberId").toString();
        return ResponseEntity.ok().body(new BaseResponse<>(notificationService.getNotification(Long.valueOf(memberId), notificationId)));
    }
}
