package com.be.gitfolio.member.dto;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
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

        @NotBlank(message = "이름은 필수 항목입니다.")
        @Size(max = 50, message = "이름은 최대 50자까지 입력할 수 있습니다.")
        private String name;

        private String avatarUrl;

        @NotBlank(message = "전화번호는 필수 항목입니다.")
        @Pattern(regexp = "^\\d{3}\\d{3,4}\\d{4}$", message = "전화번호 형식이 올바르지 않습니다. 예시: 01012345678")
        private String phoneNumber;

        @NotBlank(message = "이메일은 필수 항목입니다.")
        @Email(message = "올바른 이메일 형식이어야 합니다.")
        private String email;

        @NotBlank(message = "직군은 필수 항목입니다.")
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
