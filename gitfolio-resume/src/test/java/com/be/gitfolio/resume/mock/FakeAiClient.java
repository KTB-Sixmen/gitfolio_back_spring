package com.be.gitfolio.resume.mock;

import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.service.port.AiClient;
import com.be.gitfolio.resume.type.Template;

import java.util.List;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

public class FakeAiClient implements AiClient {

    private ResumeInfoForAiDTO resumeInfoForAiDTO;

    public void setResumeInfoForAiDTO(ResumeInfoForAiDTO resumeInfoForAiDTO) {
        this.resumeInfoForAiDTO = resumeInfoForAiDTO;
    }

    @Override
    public AIResponseDTO create(AIRequestDTO aiRequestDTO) {
        return new AIResponseDTO(
                List.of(
                        new Resume.Project(
                                "Inventory Management System",
                                "2021-05-01",
                                "2021-12-01",
                                "Java, Spring Boot, Hibernate",
                                "Developed a robust inventory management system.",
                                "https://github.com/example/inventory"
                        ),
                        new Resume.Project(
                                "Payment Gateway",
                                "2020-01-01",
                                "2020-12-31",
                                "Node.js, MongoDB, Express",
                                "Integrated a scalable payment gateway.",
                                "https://github.com/example/payment"
                        )
                ),
                List.of("Java", "Spring Boot", "React", "MySQL"),
                "I am a passionate developer skilled in backend development and modern frameworks."
        );
    }

    @Override
    public ResumeInfoForAiDTO update(AIUpdateRequestDTO aiUpdateRequestDTO) {
        return resumeInfoForAiDTO;
    }
}
