package com.be.gitfolio.resume.service;

import com.be.gitfolio.common.client.MemberGrpcClient;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.repository.ResumeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.be.gitfolio.common.grpc.MemberServiceProto.*;
import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class ResumeService {

    private final MemberGrpcClient memberGrpcClient;
    private final ResumeRepository resumeRepository;
    private final RedisTemplate<String, String> redisTemplate;


    /**
     * 이력서 생성 요청
     * TODO: 인공지능이랑 연동해야함!
     */
    @Transactional
    public String createResume(String memberId, CreateResumeRequestDTO createResumeRequestDTO) {

        // Member 가져와서 personalRepo 만들기
        MemberResponseById memberResponse = memberGrpcClient.getMember(memberId);
        String personalRepo = "http://github.com/" + memberResponse.getNickname();

        log.info("personalRepo = {}", personalRepo);
        // TODO: AI에 요청할 requestDTO 만들어서 AI에 요청하고 response와 member관련 정보 합쳐서 저장

//        String personalRepo = "Https://github.com/" + member.getNickname();

        Resume resume = Resume.of(memberId, createResumeRequestDTO);
        return resumeRepository.save(resume).getId();
    }


    /**
     * 이력서 목록 조회
     */
    public List<ResumeListDTO> getResumeList() {
        List<Resume> resumes = resumeRepository.findAll();

        return resumes.stream()
                .map(ResumeListDTO::new)
                .collect(Collectors.toList());
    }

    /**
     * 이력서 상세 조회
     */
    @Transactional
    public ResumeDetailDTO getResumeDetail(String resumeId,String clientIp) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
        incrementViewCount(resume, clientIp);
        resumeRepository.save(resume);
        return new ResumeDetailDTO(resume);
    }

    /**
     * 내 이력서 목록 조회
     */
    public List<ResumeListDTO> getMyResumeList(String memberId) {
        List<Resume> resumes = resumeRepository.findAllByMemberId(memberId);

        return resumes.stream()
                .map(ResumeListDTO::new)
                .collect(Collectors.toList());
    }

    /**
     * 이력서 삭제
     * TODO : 좋아요 삭제 / 댓글 삭제 구현해야함!
     */
    @Transactional
    public void deleteResume(String resumeId) {
        resumeRepository.deleteById(resumeId);
    }

    /**
     * 이력서 직접 수정
     */
    @Transactional
    public void updateResume(String resumeId, UpdateResumeRequestDTO updateResumeRequestDTO) {
        Resume resume = resumeRepository.findById(resumeId)
                .orElseThrow(() -> new BaseException(ErrorCode.RESUME_NOT_FOUND));
        resume.updateResume(updateResumeRequestDTO);
        resumeRepository.save(resume);
    }

    /**
     * 조회수 증가 로직
     */
    private void incrementViewCount(Resume resume, String clientIp) {
        String redisKey = "resume:view:" + resume.getId() + ":" + clientIp;

        if (Boolean.FALSE.equals(redisTemplate.hasKey(redisKey))) {
            resume.updateView(); // 조회수 증가

            // 1시간 동안 조회수 증가 안되도록 설정
            redisTemplate.opsForValue().set(redisKey, "1", 1, TimeUnit.HOURS);
            log.info("New Viewer : {}", clientIp);
        }
    }
}
