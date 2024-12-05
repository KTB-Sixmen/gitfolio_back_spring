package com.be.gitfolio.payment.controller;

import com.be.gitfolio.common.aop.AuthRequired;
import com.be.gitfolio.common.config.BaseResponse;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.service.PaymentService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;

import static com.be.gitfolio.payment.dto.PaymentRequest.*;
import static com.be.gitfolio.payment.dto.PaymentResponse.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/payments")
@Slf4j
public class PaymentController {

    @Value("${payment.success.redirectUrl}")
    private String paymentSuccessRedirectUrl;
    private final PaymentService paymentService;

    /**
     * 결제 요청
     */
    @AuthRequired
    @PostMapping("/ready")
    public ResponseEntity<BaseResponse<KakaoReadyResponse>> readyToKakaoPay(@RequestBody PaidPlanRequest paidPlanRequest,
                                              HttpServletRequest request) {
        Long memberId = (Long) request.getAttribute("memberId");
        return ResponseEntity.ok().body(new BaseResponse<>(paymentService.kakaoPayReady(memberId, paidPlanRequest)));
    }

    /**
     * 결제 성공
     */
    @GetMapping("/success")
    public void afterPayRequest(@RequestParam("pg_token") String pgToken,
                                @RequestParam("member_id") Long memberId,
                                HttpServletResponse response) throws IOException {
        paymentService.approveResponse(memberId, pgToken);
        response.sendRedirect(paymentSuccessRedirectUrl);
    }

    /**
     * 결제 진행중 취소
     */
    @GetMapping("/cancel")
    public ResponseEntity<BaseResponse<String>> cancel(@RequestParam("member_id") Long memberId,
                                                       @RequestParam("paid_plan") PaidPlan paidPlan) {
        paymentService.cancelPayment(memberId, paidPlan);
        return ResponseEntity.ok().body(new BaseResponse<>("결제가 취소되었습니다."));
    }

    /**
     * 결제 플랜 해지
     * @param request
     * @return
     */
    @PatchMapping("/terminate")
    @AuthRequired
    public ResponseEntity<BaseResponse<String>> terminate(HttpServletRequest request) {
        Long memberId = (Long) request.getAttribute("memberId");
        paymentService.terminatePayment(memberId);
        return ResponseEntity.ok().body(new BaseResponse<>("정기 결제가 해지되었습니다."));
    }

    /**
     * 결제 실패
     */
    @GetMapping("/fail")
    public ResponseEntity<BaseResponse<String>> fail(@RequestParam("member_id") Long memberId,
                                                     @RequestParam("paid_plan") PaidPlan paidPlan) {
        paymentService.failPayment(memberId, paidPlan);
        return ResponseEntity.ok().body(new BaseResponse<>("결제에 실패했습니다."));
    }
}
