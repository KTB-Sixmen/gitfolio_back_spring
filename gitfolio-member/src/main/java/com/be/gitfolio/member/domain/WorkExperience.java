package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.EmploymentStatus;
import com.be.gitfolio.common.type.WorkType;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class WorkExperience {
    private String companyName;
    private String departmentName;
    private String role;
    private WorkType workType;
    private EmploymentStatus employmentStatus;
    private String startedAt;
    private String endedAt;
}
