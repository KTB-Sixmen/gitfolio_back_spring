package com.be.gitfolio.payment.service.port;

import com.be.gitfolio.payment.dto.KakaoResponse;
import org.springframework.http.ResponseEntity;

import static com.be.gitfolio.payment.dto.KakaoResponse.*;

public interface MemberClient {

    ResponseEntity<Void> updateMemberPaidPlan(Long memberId, KakaoApproveResponse kakaoApproveResponse);

}
