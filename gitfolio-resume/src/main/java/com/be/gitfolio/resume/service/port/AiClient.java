package com.be.gitfolio.resume.service.port;

import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

public interface AiClient {

    AIResponseDTO create(AIRequestDTO aiRequestDTO);

    ResumeInfoForAiDTO update(AIUpdateRequestDTO aiUpdateRequestDTO);
}
