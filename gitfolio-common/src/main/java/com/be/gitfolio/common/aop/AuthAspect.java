package com.be.gitfolio.common.aop;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.jwt.JWTUtil;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.security.SignatureException;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Objects;

@Component
@Aspect
@Slf4j
@RequiredArgsConstructor
public class AuthAspect {

    private final JWTUtil jwtUtil;

    @Before("@annotation(com.be.gitfolio.common.aop.AuthRequired)")
    public void authenticateUser() {
        // 현재 요청의 HttpServletRequest 가져오기
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = Objects.requireNonNull(attributes, "attributes must not be null").getRequest();

        // 헤더에서 AccessToken 추출
        String authorizationHeader = request.getHeader("Authorization");

        // Authorization 헤더가 존재하는지, 그리고 Bearer로 시작하는지 확인
        if (authorizationHeader == null || !authorizationHeader.startsWith("Bearer ")) {
            throw new BaseException(ErrorCode.TOKEN_IS_MISSING);
        }

        // Bearer를 제거하고 실제 accessToken만 추출
        String accessToken = authorizationHeader.substring(7);

        // 토큰이 만료되었는지 검사
        try {
            jwtUtil.isExpired(accessToken);
        } catch (ExpiredJwtException e) {
            throw new BaseException(ErrorCode.EXPIRED_TOKEN);
        } catch (SignatureException e) {
            throw new BaseException(ErrorCode.INVALID_SIGNATURE);
        }

        // 토큰이 access인지 검사
        if (!jwtUtil.getCategory(accessToken).equals("access")) {
            throw new BaseException(ErrorCode.TOKEN_CATEGORY_INCORRECT);
        }

        // 토큰에서 사용자 정보 추출 후 request에 추가
        Long memberId = jwtUtil.getMemberId(accessToken);
        String nickname = jwtUtil.getNickname(accessToken);
        request.setAttribute("memberId", memberId);
        request.setAttribute("nickname", nickname);
        log.info("Authenticated Member ID : {}", memberId);
    }
}
