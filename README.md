# 科研绘图配色推荐器

用R语言shiny包写了一个小程序，便于学术论文绘图时，离散配色方案的选择和参考，该程序已部署至云端，欢迎访问：<a href="http://lcpmgh.com/colors/" target="_blank">科研绘图配色推荐器</a>

 线上app如有问题，仍建议下载运行，本代码开源，可在遵守MIT协议下自行取用。此外，欢迎提出意见、交流和讨论。

程序中包含的部分配色方案（保存于@palettes.csv），收集自以下文章，在此表示感谢：

1. [跟着顶刊学科研绘图——science配色篇（一）](https://zhuanlan.zhihu.com/p/679737536)

2. [跟着顶刊学科研绘图——nature配色篇（一）](https://zhuanlan.zhihu.com/p/679425898)
3. [跟着顶刊学科研绘图——nature配色篇（二）](https://zhuanlan.zhihu.com/p/679572970)
4. [跟着顶刊学科研绘图——nature配色篇（三）](https://zhuanlan.zhihu.com/p/680011321)
5. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第一期）](https://zhuanlan.zhihu.com/p/670396774)
6. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第二期）](https://zhuanlan.zhihu.com/p/674906660)
7. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第三期）](https://zhuanlan.zhihu.com/p/689959306)
8. [论文配色：跟着顶刊学配色（Nature篇）](https://zhuanlan.zhihu.com/p/704390338)

此外，推荐一些配色方案或挑选工具：

1. Python Color Palette Finder，两千多个配色方案的预览和挑选：https://python-graph-gallery.com/color-palette-finder/
2. 常见科技期刊配色colorset查询系统，一个表格，应该也是用R写的：https://www.bioinformatics.com.cn/static/others/colorsets/colors.html
3. scico, 一个R包，收集了Scientific colour maps的配色方案：https://github.com/thomasp85/scico
4. rcartocolor，一个R包，收集了CARTO的配色方案：https://jakubnowosad.com/rcartocolor/

实际上，这些配色方案太多了，看都看不过来，以我的经验，存几个自己喜欢的配色无脑用就行了。



shinyapp预览：

![shiny_app_preview](https://raw.githubusercontent.com/lcpmgh/colors/master/preview.png)



## 更新日志：

2026-06-25 v4.0

1. 大修，将原有单方案查看，更改为双栏对比布局
2. 添加了一些，包括ggsci、grDevices等R语言包的离散配色方案
3. 添加了RGB信息，优化了shiny流程，其他一些细节修改

2026-01-23 v3.0.1

1. 优化了透明度的调节选项

2026-01-16 v3.0

1. 优化代码，尤其是计算和展示流程，精简掉一些没用功能
2. 表格部分，用DT包替代reactable包，后者虽然在样式细节方面，参数丰富，但数据量大时会非常耗时
3. 经调整后，程序打开速度提升64.35%

2025-01-28 v2.0.1 

1. 添加颜色透明度调节项

2024-10-08 v2.0

1. 颜色数据库：删除重复值、无效值（获取色值时ocr识别错误）
2. 数据库详情表中添加一列预览颜色，更直观，但展示出来更耗时
3. 删除随机显示，更改为，筛选颜色数量后，点击上下项或点击数据库展示绘图效果

2024-04-05 v1.0

1. 创建程序
