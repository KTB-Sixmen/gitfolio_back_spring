package com.be.gitfolio.member.dto;

import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;


public class MemberResponseDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MemberDetailDTO {
        private Long memberId;
        private String memberAdditionalInfoId;

        private String nickname;
        private String name;
        private String username;
        private String avatarUrl;
        private String phoneNumber;
        private String email;
        private String position;

        private List<MemberAdditionalInfo.WorkExperience> workExperiences;
        private List<MemberAdditionalInfo.Education> educations;
        private List<MemberAdditionalInfo.Certificate> certificates;
        private List<MemberAdditionalInfo.Activity> activities;
        private List<MemberAdditionalInfo.Link> links;

        public static MemberDetailDTO of(Member member, MemberAdditionalInfo memberAdditionalInfo) {
            return MemberDetailDTO.builder()
                    .memberId(member.getId())
                    .memberAdditionalInfoId(memberAdditionalInfo.getMemberId())
                    .nickname(member.getNickname())
                    .name(member.getName())
                    .username(member.getUsername())
                    .avatarUrl(member.getAvatarUrl())
                    .phoneNumber(member.getPhoneNumber())
                    .email(member.getEmail())
                    .position(member.getPosition())
                    .workExperiences(memberAdditionalInfo.getWorkExperiences())
                    .educations(memberAdditionalInfo.getEducations())
                    .certificates(memberAdditionalInfo.getCertificates())
                    .activities(memberAdditionalInfo.getActivities())
                    .links(memberAdditionalInfo.getLinks())
                    .build();
        }
    }
}
