package com.be.gitfolio.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;


public class MemberDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MemberSaveRequestDTO {
        private String role;
        private String name;
        private String username;
        private String avatarUrl;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class OAuth2UserDTO {
        private Long memberId;
        private String role;
        private String name;
        private String username;
    }



}
