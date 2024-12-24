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
                        Resume.Project.builder()
                                .projectName("Payment Gateway")
                                .projectStartedAt("2020-01-01")
                                .projectEndedAt("2020-12-31")
                                .skillSet("Node.js, MongoDB, Express")
                                .roleAndTask(List.of(
                                        "Integrated a scalable payment gateway.",
                                        "Implemented secure transaction processing.",
                                        "Designed database schema for high performance."
                                ))
                                .star(Resume.Star.builder()
                                        .situation("The company needed a reliable and scalable payment processing system.")
                                        .task("To design and implement a payment gateway capable of handling high volumes of transactions.")
                                        .action("Developed a secure and efficient payment gateway using Node.js and MongoDB. Ensured high availability by implementing distributed architecture.")
                                        .result("Successfully handled 10,000 concurrent transactions, reducing transaction failures by 20%.")
                                        .build())
                                .troubleShooting(Resume.TroubleShooting.builder()
                                        .problem("Transaction failures under high load.")
                                        .hypothesis("The issue was due to unoptimized database queries and inefficient API calls.")
                                        .tring("Optimized MongoDB queries by adding proper indexing and reduced redundant API calls.")
                                        .result("Achieved a 95% success rate in transaction processing under peak loads.")
                                        .build())
                                .repoLink("https://github.com/example/payment")
                                .build(),
                        Resume.Project.builder()
                                .projectName("E-Commerce Platform")
                                .projectStartedAt("2021-06-01")
                                .projectEndedAt("2022-05-31")
                                .skillSet("Java, Spring Boot, MySQL, React")
                                .roleAndTask(List.of(
                                        "Developed a full-stack e-commerce platform.",
                                        "Implemented secure authentication and authorization with Spring Security.",
                                        "Designed and optimized relational database schema for scalability.",
                                        "Integrated third-party APIs for payment processing and shipping."
                                ))
                                .star(Resume.Star.builder()
                                        .situation("The company needed a scalable and secure platform to expand their online sales.")
                                        .task("Build a robust e-commerce platform with real-time inventory and user-friendly interfaces.")
                                        .action("Used Spring Boot to create RESTful APIs, implemented JWT-based authentication, and built a responsive frontend with React.")
                                        .result("Increased sales by 35% within the first 6 months after deployment and reduced customer complaints by 20% through better UX.")
                                        .build())
                                .troubleShooting(Resume.TroubleShooting.builder()
                                        .problem("Frequent downtime during peak hours caused by inefficient database queries.")
                                        .hypothesis("The bottleneck was due to unoptimized joins and lack of proper indexing in the database schema.")
                                        .tring("Analyzed query performance using MySQL EXPLAIN, added missing indexes, and restructured complex queries.")
                                        .result("Reduced average response time from 3 seconds to 800ms during peak traffic, improving overall system reliability.")
                                        .build())
                                .repoLink("https://github.com/example/ecommerce-platform")
                                .build()
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
