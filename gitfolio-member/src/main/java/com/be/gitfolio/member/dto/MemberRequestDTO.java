package com.be.gitfolio.member.dto;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;


public class MemberRequestDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MemberCreateRequestDTO {
        private String role;
        private String nickname;
        private String username;
        private String avatarUrl;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MemberUpdateRequestDTO {
        private String name;

        private String avatarUrl;

        private String phoneNumber;

        private String email;

        private String position;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MemberAdditionalRequestDTO {
        private List<MemberAdditionalInfo.WorkExperience> workExperiences;
        private List<MemberAdditionalInfo.Education> educations;
        private List<MemberAdditionalInfo.Certificate> certificates;
        private List<MemberAdditionalInfo.Activity> activities;
        private List<MemberAdditionalInfo.Link> links;
    }

}
