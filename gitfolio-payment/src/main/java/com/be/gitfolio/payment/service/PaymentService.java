package com.be.gitfolio.payment.service;

import com.be.gitfolio.common.client.MemberGrpcClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.config.KakaoPayProperties;
import com.be.gitfolio.payment.domain.Payment;
import com.be.gitfolio.payment.domain.PaymentStatusHistory;
import com.be.gitfolio.payment.dto.PaymentRequest;
import com.be.gitfolio.payment.dto.PaymentResponse;
import com.be.gitfolio.payment.repository.PaymentRepository;
import com.be.gitfolio.payment.repository.PaymentStatusHistoryRepository;
import com.be.gitfolio.payment.type.PaymentStatus;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

import static com.be.gitfolio.payment.dto.PaymentRequest.*;
import static com.be.gitfolio.payment.dto.PaymentResponse.*;


@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class PaymentService {

    @Value("${payment.server.url}")
    private String paymentBaseUrl;
    private final MemberGrpcClient memberGrpcClient;
    private final PaymentRepository paymentRepository;
    private final PaymentStatusHistoryRepository paymentStatusHistoryRepository;
    private final KakaoPayProperties payProperties;
    private KakaoReadyResponse kakaoReady;

    private HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        String auth = "SECRET_KEY " + payProperties.getSecretKey();
        headers.set("Authorization", auth);
        headers.set("Content-Type", "application/json");
        return headers;
    }

    /**
     * 결제 준비
     */
    @Transactional
    public KakaoReadyResponse kakaoPayReady(Long memberId, PaidPlanRequest paidPlanRequest) {
        Map<String, Object> parameters = createPaymentParameters(memberId, paidPlanRequest.getPaidPlan());
        HttpEntity<Map<String, Object>> requestEntity = new HttpEntity<>(parameters, this.getHeaders());

        // 카카오페이 결제 준비 API 호출
        kakaoReady = sendKakaoPaymentRequest(
                "https://open-api.kakaopay.com/online/v1/payment/ready",
                requestEntity,
                KakaoReadyResponse.class
        );

        Payment payment = Payment.of(memberId, paidPlanRequest, kakaoReady);
        paymentRepository.save(payment);

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.builder()
                .paymentId(payment.getId())
                .status(PaymentStatus.PENDING)
                .changedAt(LocalDateTime.now())
                .reason("결제 준비")
                .build();
        paymentStatusHistoryRepository.save(paymentStatusHistory);

        return kakaoReady;
    }

    /**
     * 결제 완료 승인
     */
    @Transactional
    public KakaoApproveResponse approveResponse(Long memberId, String pgToken) {
        KakaoApproveResponse kakaoApproveResponse = confirmKakaoPayment(memberId, pgToken);

        Payment payment = paymentRepository.findByTransactionId(kakaoApproveResponse.getTid())
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));
        payment.updatePaymentType(kakaoApproveResponse.getPayment_method_type());
        payment.updateCardInfo(kakaoApproveResponse.getCard_info());
        payment.updateStatus(PaymentStatus.APPROVED);
        paymentRepository.save(payment);

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.builder()
                .paymentId(payment.getId())
                .status(PaymentStatus.APPROVED)
                .changedAt(LocalDateTime.now())
                .reason("결제 승인")
                .build();
        paymentStatusHistoryRepository.save(paymentStatusHistory);

        memberGrpcClient.updateMemberPlan(kakaoApproveResponse.getPartner_user_id(), kakaoApproveResponse.getItem_name());

        return kakaoApproveResponse;
    }

    /**
     * 결제 취소
     */
    @Transactional
    public void cancelPayment(Long memberId, PaidPlan paidPlan) {
        Payment payment = paymentRepository.findByMemberIdAndPaidPlan(memberId, paidPlan)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.builder()
                .paymentId(payment.getId())
                .status(PaymentStatus.CANCELED)
                .changedAt(LocalDateTime.now())
                .reason("결제 취소")
                .build();
        paymentStatusHistoryRepository.save(paymentStatusHistory);
    }

    /**
     * 결제 실패
     */
    @Transactional
    public void failPayment(Long memberId, PaidPlan paidPlan) {
        Payment payment = paymentRepository.findByMemberIdAndPaidPlan(memberId, paidPlan)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.builder()
                .paymentId(payment.getId())
                .status(PaymentStatus.FAILED)
                .changedAt(LocalDateTime.now())
                .reason("결제 실패")
                .build();
        paymentStatusHistoryRepository.save(paymentStatusHistory);
    }


    // 결제 요청 파라미터 생성
    private Map<String, Object> createPaymentParameters(Long memberId, PaidPlan paidPlan) {
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("cid", payProperties.getCid());
        parameters.put("partner_order_id", "1011");
        parameters.put("partner_user_id", String.valueOf(memberId));
        parameters.put("item_name", paidPlan.getPlanName());
        parameters.put("quantity", "1");
        parameters.put("total_amount", String.valueOf(paidPlan.getCost()));
        parameters.put("tax_free_amount", "0");
        parameters.put("approval_url", paymentBaseUrl + "/api/payments/success?member_id=" + memberId);
        parameters.put("fail_url", paymentBaseUrl + "/api/payments/fail?member_id=" + memberId + "&paid_plan=" + paidPlan);
        parameters.put("cancel_url", paymentBaseUrl + "/api/payments/cancel?member_id=" + memberId + "&paid_plan=" + paidPlan);
        return parameters;
    }

    // 카카오페이 결제 승인 API 호출
    private KakaoApproveResponse confirmKakaoPayment(Long memberId, String pgToken) {
        Map<String, String> parameters = new HashMap<>();
        parameters.put("cid", payProperties.getCid());
        parameters.put("tid", kakaoReady.getTid());
        parameters.put("partner_order_id", "1011");
        parameters.put("partner_user_id", String.valueOf(memberId));
        parameters.put("pg_token", pgToken);

        HttpEntity<Map<String, String>> requestEntity = new HttpEntity<>(parameters, getHeaders());

        return sendKakaoPaymentRequest(
                "https://open-api.kakaopay.com/online/v1/payment/approve",
                requestEntity,
                KakaoApproveResponse.class
        );
    }

    // 카카오페이 API 요청 전송
    private <T> T sendKakaoPaymentRequest(String url, HttpEntity<?> requestEntity, Class<T> responseType) {
        RestTemplate restTemplate = new RestTemplate();
        return restTemplate.postForObject(url, requestEntity, responseType);
    }
}