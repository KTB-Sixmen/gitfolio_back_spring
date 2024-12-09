package com.be.gitfolio.resume.infrastructure.client;

import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.service.port.AiClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Component
public class AiClientImpl implements AiClient {

    private final WebClient webClient;

    public AiClientImpl(WebClient.Builder builder, @Value("${ai.server.url}") String baseUrl) {
        this.webClient = builder.baseUrl(baseUrl).build();
    }


    @Override
    public AIResponseDTO create(AIRequestDTO aiRequestDTO) {
        return webClient.post()
                .uri("/api/ai/resumes")
                .bodyValue(aiRequestDTO)
                .retrieve()
                .bodyToMono(AIResponseDTO.class)
                .block();
    }

    @Override
    public ResumeInfoForAiDTO update(AIUpdateRequestDTO aiUpdateRequestDTO) {
        return webClient.put()
                .uri("/api/ai/resumes")
                .bodyValue(aiUpdateRequestDTO)
                .retrieve()
                .bodyToMono(ResumeInfoForAiDTO.class)
                .block();
    }
}
