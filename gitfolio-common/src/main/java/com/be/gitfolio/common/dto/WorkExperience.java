package com.be.gitfolio.common.dto;

import com.be.gitfolio.common.type.EmploymentStatus;
import com.be.gitfolio.common.type.WorkType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
@AllArgsConstructor
public class WorkExperience {
    private String companyName;
    private String departmentName;
    private String role;
    private WorkType workType;
    private EmploymentStatus employmentStatus;
    private String startedAt;
    private String endedAt;
}
