Обновлени tdlib

стягиваем обновления
git pull
git submodule update --init --recursive
пересобираем tdlib
rm -rf build
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . -- -j$(nproc)
sudo cmake --install .
поиск старых версий tdlib
ls -l /usr/local/lib/libtdjson*
удаление старой версии
sudo rm -f /usr/local/lib/libtdjson.so.1.8.64
обновления кэша общих библиотек
sudo ldconfig
