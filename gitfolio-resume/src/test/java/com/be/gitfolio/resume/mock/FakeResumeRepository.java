package com.be.gitfolio.resume.mock;

import com.be.gitfolio.common.type.Visibility;
import com.be.gitfolio.resume.domain.Resume;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.service.port.ResumeRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.*;

import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;

public class FakeResumeRepository implements ResumeRepository {

    private List<Resume> data = new ArrayList<>();

    public List<Resume> getAll() {
        return data;
    }

    @Override
    public Page<Resume> findResumeByFilter(ResumeFilterDTO resumeFilterDTO, List<String> likedResumeId, Pageable pageable) {
        List<Resume> filteredResumes = data.stream()
                .filter(item -> Objects.equals(Visibility.PUBLIC, item.getVisibility()))
                .filter(item -> resumeFilterDTO.position() == null ||
                        Objects.equals(item.getPosition(), resumeFilterDTO.position()))
                .filter(item -> resumeFilterDTO.schoolType() == null ||
                        (item.getEducations() != null && item.getEducations().stream()
                                .anyMatch(education -> Objects.equals(education.getSchoolType(), resumeFilterDTO.schoolType()))))
                .filter(item -> likedResumeId == null || likedResumeId.contains(item.getId()))
                .toList();

        if (resumeFilterDTO.sortOrder() != null) {
            Comparator<Resume> comparator;
            switch (resumeFilterDTO.sortOrder()) {
                case "recent" -> comparator = Comparator.comparing(Resume::getUpdatedAt).reversed();
                case "like" -> comparator = Comparator.comparing(Resume::getLikeCount).reversed();
                case "view" -> comparator = Comparator.comparing(Resume::getViewCount).reversed();
                default -> comparator = Comparator.nullsFirst((a, b) -> 0); // 아무 정렬 하지 X
            }
            filteredResumes = filteredResumes.stream().sorted(comparator).toList();
        }

        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), filteredResumes.size());
        List<Resume> paginatedResumes = start > filteredResumes.size() ? List.of() : filteredResumes.subList(start, end);

        return new PageImpl<>(paginatedResumes, pageable, filteredResumes.size());
    }

    @Override
    public Resume save(Resume resume) {
        if (resume.getId() == null) {
            Resume newResume = Resume.builder()
                    .id(UUID.randomUUID().toString())
                    .memberId(resume.getMemberId())
                    .memberName(resume.getMemberName())
                    .avatarUrl(resume.getAvatarUrl())
                    .phoneNumber(resume.getPhoneNumber())
                    .email(resume.getEmail())
                    .position(resume.getPosition())
                    .techStack(resume.getTechStack())
                    .aboutMe(resume.getAboutMe())
                    .tags(resume.getTags())
                    .workExperiences(resume.getWorkExperiences())
                    .projects(resume.getProjects())
                    .links(resume.getLinks())
                    .educations(resume.getEducations())
                    .certificates(resume.getCertificates())
                    .visibility(resume.getVisibility())
                    .likeCount(resume.getLikeCount())
                    .viewCount(resume.getViewCount())
                    .template(resume.getTemplate())
                    .build();
            data.add(newResume);
            return newResume;
        } else {
            data.removeIf(item -> Objects.equals(item.getId(), resume.getId()));
            data.add(resume);
            return resume;
        }
    }

    @Override
    public Optional<Resume> findById(String resumeId) {
        return data.stream()
                .filter(item -> Objects.equals(item.getId(), resumeId)).findAny();
    }

    @Override
    public Page<Resume> findAllByMemberId(Long memberId, Pageable pageable) {
        List<Resume> filteredData = data.stream()
                .filter(item -> Objects.equals(item.getMemberId(), memberId))
                .toList();

        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), filteredData.size());
        List<Resume> paginatedData = filteredData.subList(start, end);

        return new PageImpl<>(paginatedData, pageable, filteredData.size());
    }

    @Override
    public void deleteById(String resumeId) {
        data.removeIf(item -> Objects.equals(item.getId(), resumeId));
    }
}
