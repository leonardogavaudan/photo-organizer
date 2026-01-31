export interface PhotoFile {
  id: number;
  name: string;
  path: string;
  file_type: string;
}

export interface Cluster {
  id: number;
  year: number;
  date: string;
  time_range: string;
  file_count: number;
  photo_count: number;
  screenshot_count: number;
  video_count: number;
  cluster_type: string;
  folder_name: string | null;
  files: PhotoFile[];
}
