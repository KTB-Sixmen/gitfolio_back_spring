package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.GraduationStatus;
import com.be.gitfolio.common.type.SchoolType;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class Education {
    private SchoolType schoolType;
    private String schoolName;
    private String major;
    private GraduationStatus graduationStatus;
    private String startedAt;
    private String endedAt;
}
