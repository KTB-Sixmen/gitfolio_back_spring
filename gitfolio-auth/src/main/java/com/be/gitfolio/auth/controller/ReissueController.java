package com.be.gitfolio.auth.controller;


import com.be.gitfolio.auth.infrastructure.RedisTokenRepository;
import com.be.gitfolio.common.jwt.JWTUtil;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.security.SignatureException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;

@RestController
@RequiredArgsConstructor
public class ReissueController {

    private final JWTUtil jwtUtil;
    private final RedisTokenRepository redisTokenRepository;

    @Value("${jwt.accessToken.expiry}")
    private Long accessTokenExpiry;

    @Value("${jwt.refreshToken.expiry}")
    private Long refreshTokenExpiry;


    @PostMapping("/api/auth/reissue")
    public ResponseEntity<?> reissue(HttpServletRequest request, HttpServletResponse response) {

        // Refresh 토큰 가져오기
        String refresh = null;
        Cookie[] cookies = request.getCookies();
        for (Cookie cookie : cookies) {

            if (cookie.getName().equals("refreshToken")) {

                refresh = cookie.getValue();
            }
        }

        if (refresh == null) {
            //response status code
            return new ResponseEntity<>("Refresh Token null", HttpStatus.UNAUTHORIZED);
        }

        // Refresh 토큰 만료 확인
        try {
            jwtUtil.isExpired(refresh);
        } catch (ExpiredJwtException e) {
            //response status code
            return new ResponseEntity<>("Refresh Token Expired", HttpStatus.UNAUTHORIZED);
        } catch (SignatureException e) {
            return new ResponseEntity<>("Refresh Signature Does Not Match", HttpStatus.UNAUTHORIZED);
        }

        // 토큰이 refresh인지 확인 (발급시 페이로드에 명시)
        String category = jwtUtil.getCategory(refresh);

        if (!category.equals("refresh")) {
            // response status code
            return new ResponseEntity<>("Category is NOT Refresh", HttpStatus.UNAUTHORIZED);
        }

        String username = jwtUtil.getUsername(refresh);
        String nickname = jwtUtil.getNickname(refresh);
        String role = jwtUtil.getRole(refresh);
        Long memberId = jwtUtil.getMemberId(refresh);

        // Redis에서 토큰 존재 여부 확인
        if (!redisTokenRepository.existsByRefreshToken(username)) {
            return new ResponseEntity<>("Token is NOT in Redis", HttpStatus.UNAUTHORIZED);
        }

        // make new JWT
        String newAccess = jwtUtil.createJwt("access", username, nickname, role, memberId, accessTokenExpiry);

        // refreshToken이 만료 기한의 절반보다 적게 남았을때만 재발급
        if (jwtUtil.isRefreshHaveToReissue(refresh)) {
            String newRefresh = jwtUtil.createJwt("refresh", username, nickname, role, memberId, refreshTokenExpiry);

            //Refresh 토큰 저장 DB에 기존의 Refresh 토큰 삭제 후 새 Refresh 토큰 저장
            redisTokenRepository.deleteRefreshToken(refresh);
            redisTokenRepository.saveRefreshToken(username, newRefresh, refreshTokenExpiry);

            // 쿠키와 헤더에 토큰 설정
            response.addCookie(createCookie("refreshToken", newRefresh));
        }


        // AccessToken을 body에 담아서 반환
        HashMap<String, String> tokens = new HashMap<>();
        tokens.put("accessToken", newAccess);

        return new ResponseEntity<>(tokens, HttpStatus.OK);
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
