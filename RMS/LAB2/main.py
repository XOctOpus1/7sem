import sys
import os
from PyQt5.QtWidgets import QApplication, QWidget, QHBoxLayout, QVBoxLayout, QPushButton, QSlider, QLabel, QFileDialog
from PyQt5.QtMultimedia import QMediaPlayer, QMediaContent
from PyQt5.QtCore import Qt, QUrl
import pygame


class MediaPlayer(QWidget):
    def __init__(self):
        super().__init__()

        pygame.init()
        self.clock = pygame.time.Clock()

        self.playing = False
        self.current_time = 0
        self.current_index = 0
        self.playlist = []

        self.media_player = QMediaPlayer()

        self.init_ui()

    def init_ui(self):
        self.play_button = QPushButton('Play/Pause')
        self.stop_button = QPushButton('Stop')
        self.previous_button = QPushButton('Previous')
        self.next_button = QPushButton('Next')
        self.seek_slider = QSlider(Qt.Horizontal)
        self.volume_slider = QSlider(Qt.Horizontal)
        self.time_label = QLabel('0:00 / 0:00')

        layout = QVBoxLayout()
        button_layout = QHBoxLayout()

        button_layout.addWidget(self.previous_button)
        button_layout.addWidget(self.play_button)
        button_layout.addWidget(self.next_button)
        button_layout.addWidget(self.stop_button)

        layout.addLayout(button_layout)
        layout.addWidget(self.seek_slider)
        layout.addWidget(self.volume_slider)
        layout.addWidget(self.time_label)

        self.setLayout(layout)

        self.play_button.clicked.connect(self.play_pause)
        self.stop_button.clicked.connect(self.stop)
        self.seek_slider.sliderMoved.connect(self.set_position)
        self.volume_slider.sliderMoved.connect(self.set_volume)
        self.previous_button.clicked.connect(self.previous_track)
        self.next_button.clicked.connect(self.next_track)

        self.media_player.positionChanged.connect(self.update_position)
        self.media_player.durationChanged.connect(self.update_duration)

        # Set the initial volume for both pygame and QMediaPlayer
        initial_volume = 50  # You can set the initial volume as needed
        self.volume_slider.setValue(initial_volume)
        self.set_volume(initial_volume)

        # Add a button to open a file dialog for selecting a folder
        self.open_button = QPushButton('Open Folder')
        self.open_button.clicked.connect(self.open_folder_dialog)
        layout.addWidget(self.open_button)

    def open_folder_dialog(self):
        options = QFileDialog.Options()
        options |= QFileDialog.DontUseNativeDialog
        folder_path = QFileDialog.getExistingDirectory(self, "Open Folder", options=options)

        if folder_path:
            self.playlist = [os.path.join(folder_path, file) for file in os.listdir(folder_path) if
                             file.lower().endswith(('.mp3', '.ogg', '.wav'))]
            if self.playlist:
                self.current_index = 0
                self.media_player.setMedia(QMediaContent(QUrl.fromLocalFile(self.playlist[self.current_index])))
                self.play_button.setEnabled(True)
                self.update_time_label()

    def play_pause(self):
        if not self.media_player.media().isNull():
            if self.playing:
                pygame.mixer.music.pause()
                self.media_player.pause()
            else:
                pygame.mixer.music.unpause()
                self.media_player.play()

            self.playing = not self.playing

    def stop(self):
        pygame.mixer.music.stop()
        self.media_player.stop()
        self.playing = False

    def set_position(self, position):
        pygame.mixer.music.set_pos(position / 1000.0)
        self.media_player.setPosition(position)

    def set_volume(self, volume):
        pygame.mixer.music.set_volume(volume / 100.0)
        self.media_player.setVolume(volume)

    def update_position(self, position):
        self.seek_slider.setValue(position)
        self.current_time = position / 1000
        self.update_time_label()

    def update_duration(self, duration):
        self.seek_slider.setRange(0, duration)

    def update_time_label(self):
        total_time = self.media_player.duration() / 1000
        self.time_label.setText(f'{self.format_time(self.current_time)} / {self.format_time(total_time)}')

    def format_time(self, time_seconds):
        minutes, seconds = divmod(int(time_seconds), 60)
        return f'{minutes}:{seconds:02}'

    def previous_track(self):
        if len(self.playlist) > 1:
            self.current_index = (self.current_index - 1) % len(self.playlist)
            self.media_player.setMedia(QMediaContent(QUrl.fromLocalFile(self.playlist[self.current_index])))
            self.play_pause()

    def next_track(self):
        if len(self.playlist) > 1:
            self.current_index = (self.current_index + 1) % len(self.playlist)
            self.media_player.setMedia(QMediaContent(QUrl.fromLocalFile(self.playlist[self.current_index])))
            self.play_pause()


if __name__ == '__main__':
    app = QApplication(sys.argv)
    player = MediaPlayer()
    player.show()
    sys.exit(app.exec_())
