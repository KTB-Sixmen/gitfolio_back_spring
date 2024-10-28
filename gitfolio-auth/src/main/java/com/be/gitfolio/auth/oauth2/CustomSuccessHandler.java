package com.be.gitfolio.auth.oauth2;

import com.be.gitfolio.auth.dto.CustomOAuth2User;
import com.be.gitfolio.auth.repository.RedisTokenRepository;
import com.be.gitfolio.common.jwt.JWTUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

@Component
@RequiredArgsConstructor
public class CustomSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

    private final JWTUtil jwtUtil;
    private final RedisTokenRepository redisTokenRepository;

    @Value("${jwt.refreshToken.expiry}")
    private Long refreshTokenExpiry;

    @Value("${jwt.redirect.mainPageUrl}")
    private String mainPageUrl;

    @Value("${jwt.redirect.onBoardingPageUrl}")
    private String onboardingPageUrl;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {

        //OAuth2User
        CustomOAuth2User customUserDetails = (CustomOAuth2User) authentication.getPrincipal();
        String username = customUserDetails.getUsername();
        String nickname = customUserDetails.getName();
        Long memberId = customUserDetails.getMemberId();

        Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();
        Iterator<? extends GrantedAuthority> iterator = authorities.iterator();
        GrantedAuthority auth = iterator.next();
        String role = auth.getAuthority();

        //토큰 생성
        String refresh = jwtUtil.createJwt("refresh", username, nickname, role, memberId,refreshTokenExpiry);

        //Refresh 토큰 저장
        redisTokenRepository.saveRefreshToken(username, refresh, refreshTokenExpiry);

        //응답 설정
        response.addCookie(createCookie("refreshToken", refresh));

        // 신규 사용자 여부 확인
        String redirectUrl = customUserDetails.getIsNewMember() ? onboardingPageUrl : mainPageUrl;

        response.setStatus(HttpStatus.OK.value());
        response.sendRedirect(redirectUrl);
    }

    private Cookie createCookie(String key, String value) {
        Cookie cookie = new Cookie(key, value);
        cookie.setMaxAge(24*60*60);
        //cookie.setSecure(true);
        cookie.setPath("/");
        cookie.setHttpOnly(true);

        return cookie;
    }
}
