package com.be.gitfolio.member.domain.request;

import com.be.gitfolio.common.type.PositionType;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Builder;


public class MemberRequest {

    @Builder
    public record MemberCreate(
            String role,
            String nickname,
            String username,
            String avatarUrl,
            String githubName
    ) {}

    @Builder
    public record MemberUpdate(
            @NotBlank(message = "이름은 필수 항목입니다.")
            @Size(max = 50, message = "이름은 최대 50자까지 입력할 수 있습니다.")
            String name,
            @NotBlank(message = "전화번호는 필수 항목입니다.")
            @Pattern(regexp = "^\\d{3}\\d{3,4}\\d{4}$", message = "전화번호 형식이 올바르지 않습니다. 예시: 01012345678")
            String phoneNumber,
            @NotBlank(message = "이메일은 필수 항목입니다.")
            @Email(message = "올바른 이메일 형식이어야 합니다.")
            String email,
            PositionType position
    ) {}
}